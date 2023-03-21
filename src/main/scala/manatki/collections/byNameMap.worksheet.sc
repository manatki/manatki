val bnMap = Map(
    "ololo" -> (() => {println("ololo"); 1}),
    "kek" -> (() => {println("kek"); 2})
)

val normalMap = bnMap.view.mapValues(_())

normalMap("ololo")
normalMap("kek")