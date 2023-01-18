object assignment {
  def main(args: Array[String]): Unit = {
    /*
   Zadanie:
     pokyny:
       - indexuje sa od 0
       - immutable
       - odovzdava sa struktura ulozena vo "val nazov ..." nie println
       - zadanie needitovat
       - podla vysledku bud zapisem alebo nazapisem novu znamku, predosla pisomka ostava zapisana
     dovysvetlim:
       - maxBy
       - groupBy
       - modulo a delenie
       - s"$a,$b,$c" pattern matching, pozor na poradie pri matchovani
       - println(screen.split("\n").toList)
       - line.replace(" ", "")
     1, v premennej screen su pixely v tvare:
         index:red,green,blue,transparency
        kde transparency je optional, tj. su dva druhy pixelov, s priehladnostou a bez nej
        v premennej dimension je hodnost matice, ktoru treba zo struktury vytvorit, teda pre 2 je to matica 2x2 atd
        vysledok tejto ulohy je struktura:
         val pixels: List[Pixel], kde trieda Pixel obsahuje svoju poziciu v matici,
           napr prvok s indexom 3 bude mat poziciu (0, 1) (x,y),
           dalej pixel obsahuje informaciu o farbe a priehladnosti
        priklad (vytlaceny list):
         |TransparentPixel(Pos(0,0),Color(100,56,125),200)|RGBPixel(Pos(1,0),Color(255,0,0))|RGBPixel(Pos(2,0),Color(0,200,135))
         |TransparentPixel(Pos(0,1),Color(220,0,12),100)|RGBPixel(Pos(1,1),Color(45,0,97))|RGBPixel(Pos(2,1),Color(0,0,0))
         |TransparentPixel(Pos(0,2),Color(0,0,0),0)|RGBPixel(Pos(1,2),Color(0,0,0))|RGBPixel(Pos(2,2),Color(255,255,255))
     2, pre kazdy riadok v matici pixelov najdi najcervensi pixel (tj. ten s najvacsou R zlozkou)
        vysledok tejto ulohy je mapa kde kluc je cislo riadku a hodnota najdeny pixel
        priklad:
         Map(
           0 -> RGBPixel(Pos(1,0),Color(255,0,0)),
           1 -> TransparentPixel(Pos(0,1),Color(220,0,12),100),
           2 -> RGBPixel(Pos(2,2),Color(255,255,255))
         )
     3, najdi poziciu najprehladnejsieho pixelu (tj. ma najvacsiu transparency) v celej matici
        vysledok tejto ulohy je dvojica (x, y)
        priklad:
         Pos(0,0)
    */

    case class Pos(x: Int, y: Int)
    case class Color(r: Int, g: Int, b: Int)
    trait Pixel {
      val pos: Pos
      val color: Color
    }
    case class TransparentPixel(pos: Pos, color: Color, transparency: Int) extends Pixel
    case class RGBPixel(pos: Pos, color: Color) extends Pixel

    val dimension = 3
    val screen =
      """|0:100,125, 56,200
         |1:255,  0,  0
         |2:  0,135,200
         |3:220, 12,  0,100
         |4: 45, 97,  0
         |5:  0,  0,  0
         |6:  0,  0,  0,0
         |7:  0,  0,  0
         |8:255,255,255
         |""".stripMargin

    val pixels: List[Pixel] = screen.replace(" ", "").split("\n").toList.map {
      case s"$index:$r,$g,$b,$transparency" => TransparentPixel(pos=Pos(index.toInt % dimension, index.toInt / dimension), color=Color(r.toInt, g.toInt, b.toInt), transparency=transparency.strip().toInt)
      case s"$index:$r,$g,$b" => RGBPixel(pos = Pos(index.toInt % dimension, index.toInt / dimension), color = Color(r.toInt, g.toInt, b.strip().toInt))
    }

    val highestRed: Map[Int, Pixel] = pixels.groupBy(_.pos.y).map{ case (index, pixel) => (index, pixel.maxBy(_.color.r))}
    val getTransparentPixels: List[TransparentPixel] = pixels.collect{ case transparentPixel: TransparentPixel => transparentPixel}

  }
}