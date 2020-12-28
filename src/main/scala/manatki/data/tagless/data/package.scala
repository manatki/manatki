package manatki.data.tagless

package object data {
  type XSin[+a]    = Layer[Single.SP[a, -*, +*]]
  type XNil        = Layer[Nil.NP]
  type XStream[+a] = Layer[Cons[a, -*, +*]]
  type XOpt[+a]    = Layer[OptP.OP[a, -*, +*]]
  type XList[+a]   = ListP.T[a]
  type XNel[+a]    = Layer[NelP[a, -*, +*]]
}
