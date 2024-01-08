package manatki.poc.multiDomain
import cats.Id
import cats.data.ReaderT
import cats.effect.IO
import cats.effect.Ref
import cats.implicits._
import manatki.poc.multiDomain.FileSystem._

object FileSystem {
  sealed trait FS

  final abstract class Ident   extends FS
  final abstract class Content extends FS
  sealed trait Node            extends FS

  final abstract class Dir  extends Node
  final abstract class File extends Node
  final abstract class Link extends Node
}

trait Directories[D[_ <: FS], F[_]] {
  def contents(d: D[Dir]): F[List[D[Ident]]]

  def add(d: D[Dir], id: D[Ident], node: D[Node]): F[D[Dir]]
}

trait Nodes[D[_ <: FS], F[_]] {
  def determine(d: D[Node]): F[Either[D[Link], Either[D[Dir], D[File]]]]
  def follow(link: D[Link]): F[D[Node]]
}

trait Files[D[_ <: FS], F[_]] {
  def read(file: D[File]): F[D[Content]]
  def write(file: D[File], content: D[Content]): F[D[File]]
  def append(file: D[File], content: D[Content]): F[D[File]]
}

/** immutable system does not support links */
sealed trait AFS[+fs <: FS]
object AFS {

  sealed trait TreeFS[+fs <: Node]                    extends AFS[fs]
  case class ADir(content: Map[String, TreeFS[Node]]) extends TreeFS[Dir]
  case class AFile(content: String)                   extends TreeFS[File]

  final case class AId(name: String)      extends AFS[Ident]
  final case class ACont(content: String) extends AFS[Content]

  final case class ALink(absurd: Nothing) extends AFS[Link]

  implicit object directories extends Directories[AFS, Id] {
    def contents(dir: AFS[Dir]): List[AFS[Ident]] = dir match {
      case d @ ADir(cs) => cs.keys.map(AId).toList
    }
    def add(dir: AFS[Dir], id: AFS[Ident], node: AFS[Node]): AFS[Dir] = (dir, id, node) match {
      case (ADir(cs), AId(name), fsnode: TreeFS[Node]) => ADir(cs + (name -> fsnode))
      case (_, _, ALink(absurd))                       => absurd
    }
  }

  implicit object nodes extends Nodes[AFS, Id] {
    def determine(node: AFS[Node]): Id[Either[AFS[Link], Either[AFS[Dir], AFS[File]]]] =
      Right(node match {
        case dir @ ADir(_)   => Left(dir)
        case file @ AFile(_) => Right(file)
        case ALink(absurd)   => absurd
      })
    def follow(link: AFS[Link]): Id[AFS[Node]] = link match {
      case ALink(absurd) => absurd
    }
  }

  implicit object files extends Files[AFS, Id] {
    def read(file: AFS[File]): AFS[Content] = file match {
      case AFile(content) => ACont(content)
    }
    def write(file: AFS[File], content: AFS[Content]): AFS[File] = content match {
      case ACont(content) => AFile(content)
    }
    def append(file: AFS[File], content: AFS[Content]): AFS[File] = (file, content) match {
      case (AFile(c1), ACont(c2)) => AFile(c1 ++ c2)
    }
  }
}

sealed trait IOFS[fs <: FS]

object IOFS {

  type ByteString = IndexedSeq[Byte]

  trait FSSstate {
    def id: Ref[IO, Long]
    def dirs: Ref[IO, Map[Long, List[Long]]]
    def files: Ref[IO, Map[Long, ByteString]]
    def links: Ref[IO, Map[Long, Long]]
  }

  type Eff[a] = ReaderT[IO, FSSstate, a]

  case class IODir(id: Long)             extends IOFS[Dir]
  case class IOFile(id: Long)            extends IOFS[File]
  case class IOLink(id: Long)            extends IOFS[Link]
  case class IOName(id: Long)            extends IOFS[Ident]
  case class IOCont(content: ByteString) extends IOFS[Content]
  case class IONode(id: Long)            extends IOFS[Node]

  final case class IOError(message: String) extends RuntimeException

  implicit object directories extends Directories[IOFS, Eff] {
    def contents(d: IOFS[Dir]): Eff[List[IOFS[Ident]]] = ReaderT(
      state =>
        d match {
          case IODir(id) =>
            for {
              m   <- state.dirs.get
              res <- m.get(id).map(_.map(IOName)).liftTo[IO](IOError(s"directory not found : $id"))
            } yield res
        }
    )
    def add(d: IOFS[Dir], id: IOFS[Ident], node: IOFS[Node]): Eff[IOFS[Dir]] =
      ReaderT(
        state =>
          (d, node) match {
            case (IODir(did), IONode(nid)) =>
              state.dirs.update(m => m + (did -> (nid :: m.getOrElse(did, Nil)))).as(d)
          }
      )
  }

  implicit object nodes extends Nodes[IOFS, Eff] {
    def determine(d: IOFS[Node]): Eff[Either[IOFS[Link], Either[IOFS[Dir], IOFS[File]]]] =
      d match {
        case IONode(id) =>
          ReaderT { state =>
            state.links.get
              .map(_.contains(id))
              .ifM(
                Left(IOLink(id)).pure[IO],
                state.dirs.get
                  .map(_.contains(id))
                  .ifM(
                    Right(Left(IODir(id))).pure[IO],
                    state.files.get
                      .map(_.contains(id))
                      .ifM(
                        Right(Right(IOFile(id))).pure[IO],
                        IO.raiseError(IOError(s"not found node : $id"))
                      )
                  )
              )
          }
      }
    def follow(link: IOFS[Link]): Eff[IOFS[Node]] = link match {
      case IOLink(id) =>
        ReaderT(state => state.links.get.flatMap(m => m.get(id).map(IONode).liftTo[IO](IOError(s"unknown link $id"))))
    }
  }

  implicit object files extends Files[IOFS, Eff] {
    def read(file: IOFS[File]): Eff[IOFS[Content]] = file match {
      case IOFile(id) =>
        ReaderT(state => state.files.get.flatMap(_.get(id).map(IOCont).liftTo[IO](IOError(s"file not found: $id"))))
    }
    def write(file: IOFS[File], content: IOFS[Content]): Eff[IOFS[File]] =
      (file, content) match {
        case (IOFile(id), IOCont(barr)) =>
          ReaderT(
            state => state.files.update(m => m + (id -> barr)).as(file)
          )
      }
    def append(file: IOFS[File], content: IOFS[Content]): Eff[IOFS[File]] = (file, content) match {
      case (IOFile(id), IOCont(barr)) =>
        ReaderT(
          state =>
            state.files
              .update(
                m => m + (id -> (m.getOrElse(id, IndexedSeq.empty) ++ barr))
              )
              .as(file)
        )
    }
  }
}
