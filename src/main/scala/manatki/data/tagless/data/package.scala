package manatki.data.tagless

package object data {
  type XSin[+a]    = Layer[Single.NP[a, -*, +*]]
  type XNil        = Layer[Nil.NP]
  type XStream[+a] = Layer[Cons[a, -*, +*]]
  type XOpt[+a]    = Layer[OptP.OP[a, -*, +*]]
  type XList[+a]   = Layer[ListP[a, -*, +*]]
  type XNel[+a]    = Layer[NelP[a, -*, +*]]
}
