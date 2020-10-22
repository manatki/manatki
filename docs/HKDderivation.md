# HKD derivation

This is a design draft for the upcoming generic derivation for higher kinded datatypes (HKD)

## Motivation

Having such a datatype definition 

```scala
case class Product[F[_]](
    name: F[String],
    category: F[Option[Category]],
    price: F[BigDecimal]
)
```

one can achieve a lot of helpful things over than just `case class` without `F[_]`, e.g.  
- original data is restorable as `Product[Id]` 
- `Product[Option]` could work as a functional builder,
- `Product[Either[String, A]]` as a validation result, 
- `Product[IO]` and `Product[Ref[IO, *]]` could be used as a view and state of reactive configuration,
- `Product[Stream[IO, *]]` could work as a reactive dataframe with good types, etc. 

HKD Patterns should be described in some posts soon.


A good typeclass equipment is crucial to make the future bright for the HKD. So one needs some essential type classes and a lot of utility type classes. Latter means we need some generic derivation library such as magnolia is for simple types.

## Essential typeclasses

Several type classes may and should be derived for any HKD in the simplest form.

### RepresentableK

As for higher kinded traits, HKD could greatly benefit from the representability, having the following definitions

```scala 
trait FunctionK[-F[_], +G[_]]{
    def apply[A](fa: F[A]): G[A]
}


trait RepK[-U[f[_]], A]{
    def apply[F[_]](uf: U[F]): F[A]
}


trait RepresentableK[U[f[_]]]{
    def tabulate[F[_]](fr: FunctionK[RepK[U, *], F]): U[F]
}
```


instance for the `Product` could be given trivially using `tofu.higherKind` helpers :

```scala
implicit val productRepresentable: RepresentableK[Product] =
  new RepresentableK[Product] {
    def tabulate[F[_]](fr: FunctionK[RepK[Product, *], F]): Product[F] =
      Product(
        name = fr(RepK[Product](_.name)),
        category = fr(RepK[Product](_.category)),
        price = fr(RepK[Product](_.price)),
      )
  }
```

`RepresentableK[U]` instance means the type `U`  has `F` effectively on the "result" positions, i.e. every `U` field or method result is `F[SomeType]` and this is the only place where `F` is met. "effectively" means that some result types or fields could have type `V[F]` where `V` is a representable itself.
It automatically gives us `Embed`, `FunctorK`, `ApplyK` instances, etc.

Representablity means type could be understood as a large product, but how large - we don't know.
So we still can have infinite products in form `type U[F] = A => F[SomeType]` which are still representable

### TraverseK

To limit previous ability we add another type class which can not be derived for usual higher kinded traits

```scala
trait SequenceK[U[f[_]]] {
  type Composed[F[_], G[_], A] = F[G[A]]

  def sequenceK[F[_]: Applicative, G[_]](uf: U[Composed[F, G, *]]): F[U[G]]
  
  def sequenceIdK[F[_]: Applicative](uf: U[F]): F[U[Id]] = sequenceK[F, Id](uf)
}
```

again instance for `Product` is definet trivially using the `cats.syntax.apply` extensions

```scala
implicit val productSequence: SequenceK[Product] =
  new SequenceK[Product] {
    def sequenceK[F[_]: Applicative, G[_]](uf: Product[Composed[F, G, *]]): F[Product[G]] =
      (uf.name, uf.category, uf.price).mapN(Product.apply[G])
  }
```

This means we can enumerate all the fields that have `F` in the result.
`SequenceK` still can be defined for constant type constructor such as `type StringT[F[_]] = String` which means that `SequenceK


This definition could be combined with `RepresentableK` into a powerful single abstract method type class

```scala
trait TraverseK[U[f[_]]] extends RepresentableK[U] with SequenceK[U] {
  type Composed[F[_], G[_], A] = F[G[A]]
  def traverseK[F[_]: Applicative, G[_]](fr: FunctionK[RepK[U, *], Composed[F, G, *]]): F[U[G]]

  def tabulate[F[_]](hom: FunctionK[RepK[U, *], F]): U[F] =
    traverseK[Id, F](hom)

  def sequenceK[F[_]: Applicative, G[_]](uf: U[Composed[F, G, *]]): F[U[G]] =
    traverseK[F, G](funK(rep => rep(uf)))
}
```

Next, we again can verify that instance for the `Product` is constructed trivially

```scala
implicit val productTraverse: TraverseK[Product] =
  new TraverseK[Product] {
    def traverseK[F[_]: Applicative, G[_]](fr: FunctionK[RepK[Product, *], Composed[F, G, *]]): F[Product[G]] =
      (
        fr(RepK[Product](_.name)),
        fr(RepK[Product](_.category)),
        fr(RepK[Product](_.price))
      ).mapN(Product.apply[G])
  }
```


### FieldInfo

Finally, we can add type classes that have a basic description for each field. One possible way is the following

```scala
case class FieldInfo[A](name: String, annotations: Vector[AnyRef] = Vector.empty)

type HKDInfo[U[f[_]]] = U[FieldInfo]
```

Since or datatype has no annotation we can simply name each field to give such an instance

```scala
implicit val productInfo: HKDInfo[Product] = 
  Product[Lambda[A => String]](
      name = "name",
      category = "category",
      price = "price"  
  ).mapK(funK(s => FieldInfo(s)))
```

We desire that all `HKDInfo` and `TraverseK` would be derived by some macro, preferably using macro-annotations

## Utility typeclasses

Magnolia is great for simple cases, but its internal mechanics are too advanced to support automatic derivation, while we'd like to support only semi-automatic.
Magnolia has several shortcomings:

- Each derivation requires the same typeclass for each field type
- There is no ability not to require typeclass instance, based on the annotation
- It cannot support higher kinded types
- It has some issues with cross-recursive data types
- It provides some type safety, but it's limited and most of the useful typeclasses require unsafe operations on `Any` instances

Hence we decide to provide alternative derivation mechanics, exploiting HKD ability to describe itself.

### Derivation description

Base abstracion for derivation provider is 
```scala
trait HKDDerivation[TC[u[f[_]]], Field[_]] {
  def deriveWith[U[f[_]]](instances: => U[Field])(implicit traverse: TraverseK[U], info: HKDInfo[U]): TC[U]
}
```
Here our provider would require a `Field` typeclass for each of field type. 
The whole pack of `Field` instances will be assembled into the `U[Field]` value and provided to our provider.
Note that instances are passed by name which allows for complex recursion forms.
Moreover, the provider may require basic `TraverseK` and `HKDInfo` instances as they should be generated for each HKD definition in the first place.


Macro for the derivation would have the following signature

```scala
def deriveInstance[U[f[_]], TC[u[f[_]]], Field[_]](derivator: HKDDerivation[TC, Field]): TC[U] = macro ...
```

This macro function will be provided by the library, so users won't need to define their macros for each typeclass.
The code generated would be extremely simple. Let's imagine we have such typeclass 
```scala
trait HKDIdShow[U[f[_]]] {
  def encode(uid: U[Id]): String
}
``` 
which is the simplest form of show

invocation of `deriveInstance[Product, HKDIdShow, Show]` will produce an equivalent of the following definition

```scala
def deriveInstanceShowProduct(derivator: HKDDerivation[HKDIdShow, Show]): HKDIdShow[Product] =
  derivator.deriveWith(
    Product[Show](
      name = implicitly,
      category = implicitly,
      price = implicitly
    )
  )
```

This code will require `Show` for each instance, also `TraverseK` and `FieldInfo` for the HKD definition while being extremely simple.

### Case Study: JSON

Let's imagine how `Decoder` and `Encoder` providers should look for us
We'll try to achieve the complex task: derive codec for `HKD[F]` whenever `F` itself can provide the codec.

We'll start from the typeclasses for such reprovision

```scala
trait EncoderK[F[_]] {
  implicit def encoder[A: Encoder]: Encoder[F[A]]
}

trait DecoderK[F[_]] {
  implicit def decoder[A: Decoder]: Decoder[F[A]]
}
```
We can see a pattern here, so we can derive more general mechanics

```scala
trait WrapInstance[F[_], TC[_]] {
  implicit def wrap[A: TC]: TC[F[A]]
}

type EncoderK[F[_]] = WrapInstance[F, Encoder]
type DecoderK[F[_]] = WrapInstance[F, Encoder]
```

These instances could be defined for all the standard containers: `Option`, `List`, `Either[E, *`] where `E` has Codec, etc...

Now we can define our typeclasses

```scala

trait HKDEncoder[U[f[_]]] {
  def encoder[F[_]: EncoderK]: Encoder[U[F]]
}

trait HKDDecoder[U[f[_]]] {
  def decoder[F[_]: DecoderK]: Decoder[U[F]]
}
```

Another obvious pattern and another generalization

```scala
trait WrapHKD[U[f[_]], TC[_]]{
    implicit def wrapHKD[F[_]](implicit wrap: WrapInstance[F, TC]): TC[U[F]]
}

type HKDEncoder[U[f[_]]] = WrapHKD[U, Encoder]

type HKDDecoder[U[f[_]]] = WrapHKD[U, Decoder]
```

So having `HKDEncoder` and `HKDDecoder` for `Product` means we automatically get `Encoder` and `Decoder` for `Product[Id]`, `Product[Option]`, `Product[Either[String,*]]`, `Product[Vector]`, etc...

The only step is writing providers for such typeclasses using our previous abstraction `HKDDerivation`

Here's the encoder
```scala
implicit val hkdEncoderDerive: HKDDerivation[HKDEncoder, Encoder] =
  new HKDDerivation[HKDEncoder, Encoder] {
    def deriveWith[U[f[_]]](
        instances: => U[Encoder]
    )(implicit traverse: TraverseK[U], info: HKDInfo[U]): HKDEncoder[U] =
      new HKDEncoder[U] {
        implicit def wrapHKD[F[_]](implicit wrap: WrapInstance[F, Encoder]): Encoder[U[F]] =
          new Encoder[U[F]] {
            def apply(data: U[F]): Json = {
              val jsons: U[Lambda[A => Json]]                   =
                data.zipWithK(instances)(Function2K.apply { (field, instance) =>
                  wrap.wrap(instance).apply(field)
                })
              val withName: U[Const[Vector[(String, Json)], *]] =
                jsons.zipWithK(info)(Function2K.apply { (json, finfo) =>
                  Const(Vector(finfo.name -> json))
                })

              Json.obj(traverse.sequenceIdK(withName).getConst: _*)
            }
          }
      }
  }
```

and the encoder
```scala
implicit val hkdDecoderDerive: HKDDerivation[HKDDecoder, Decoder] =
  new HKDDerivation[HKDDecoder, Decoder] {
    def deriveWith[U[f[_]]](
        instances: => U[Decoder]
    )(implicit traverse: TraverseK[U], info: HKDInfo[U]): HKDDecoder[U] =
      new HKDDecoder[U] {
        implicit def wrapHKD[F[_]](implicit wrap: WrapInstance[F, Decoder]): Decoder[U[F]] =
          new Decoder[U[F]] {
            def apply(c: HCursor): Decoder.Result[U[F]] = {
              val results: U[Lambda[A => Decoder.Result[F[A]]]] =
                info.zipWithK(instances)(Function2K { (finfo, instance) =>
                  c.downField(finfo.name).as(wrap.wrap(instance))
                })
              traverse.sequenceK[Decoder.Result, F](results)
            }
          }
      }
  }
  ```

These definitions look like big onions - three anonymous classes inside each other.
Still, it's interesting how everything is needed for final encoding, decoding is getting to the scope step by step.
And the elegance of such an approach is how there is no need for unsafe operations. 
Every step is full type-checked and you can write, guided by types.

These values are replacement for magnolias `combine` method, but they are fully checked for compatibility before your user will try to derive any instance since they are merely trait implementations.


