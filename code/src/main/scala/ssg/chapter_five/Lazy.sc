import ssg.chapter_five._

val s: Stream[Int] = Stream(1,2,3,4,5)

s.toList
val one = Stream.ones.take(5)

s.takeWhile(_ < 5).toList

val s2 = s.take(4).toList
s2.take(3).toList

s2.drop(2)

s.map(_+1).toList
s.filter(_%2 == 1).toList

s.append(one).toList


Stream.constant("a").take(5).toList

Stream.fibu().take(10).toList
Stream.fromu(5).take(10).toList
Stream.constantu("a").take(10).toList
Stream.onesu().take(10).toList

s.takeWhileu(_ < 5).toList

s.zipAll(Stream.ones.take(15)).take(20).toList

val s3 = Stream(1,2,3)


s3.startsWith(s)