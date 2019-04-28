package cs320

package object hw01 extends Homework01 {
  // Problem 1
  def dollar2won(dollar: Int): Int = {
    dollar * 1100
  }
  def volumeOfCuboid(a: Int, b: Int, c: Int): Int = {
    a * b * c
  }
  def isEven(num: Int): Boolean = {
    num % 2 == 0
  }
  def isOdd(num: Int): Boolean = {
    num % 2 == 1
  }
  def gcd(a: Int, b: Int): Int = {
    if (a<b) {
      return gcd(b,a)
    }
    val q = a/b
    val r = a%b
    if (r==0){
      b
    }else{
      gcd(b, r)
    }
  }
  def lcm(a: Int, b: Int): Int = {
    a / gcd(a,b) * b
  }

  // Problem 2
  def numOfHomework(course: COURSE): Int = course match {
    case CS320(quiz, homework) => homework
    case CS311(homework) => homework
    case CS330(projects, homework) => homework

  }
  def hasProjects(course: COURSE): Boolean = course match {
    case CS320(quiz, homework) => false
    case CS311(homework) => false
    case CS330(projects, homework) => true
  }

  // Problem 3
  def namePets(pets: List[String]): List[String] = {
    //mapMap("dog"->"happy","cat"->"smart","pig"->"pinky")
    val step1: List[String] = pets.map((species : String) => if (species == "dog") "happy" else species)
    val step2: List[String] = step1.map((species : String) => if (species == "cat") "smart" else species)
    val step3: List[String] = step2.map((species : String) => if (species == "pig") "pinky" else species)
    step3
  }
  def giveName(oldName: String, newName: String): List[String] => List[String] = {
    pets: List[String] => pets.map((species : String) => if (species == oldName) newName else species)
  }

  def tests: Unit = {
    // 1. tests for dollar2won
    test(dollar2won(1), 1100)
    test(dollar2won(2), 2200)
    test(dollar2won(3), 3300)
    test(dollar2won(4), 4400)
    test(dollar2won(5), 5500)
    test(dollar2won(6), 6600)
    test(dollar2won(7), 7700)
    test(dollar2won(123), 135300)
    test(dollar2won(150), 165000)
    test(dollar2won(123123), 135435300)

    // 2. tests for volumeOfCuboid
    test(volumeOfCuboid(1, 2, 3), 6)
    test(volumeOfCuboid(4, 5, 7), 140)
    test(volumeOfCuboid(6, 3, 10), 180)
    test(volumeOfCuboid(2, 4, 3), 24)
    test(volumeOfCuboid(10, 20, 30), 6000)
    test(volumeOfCuboid(100, 200, 300), 6000000)
    test(volumeOfCuboid(1, 1, 1), 1)
    test(volumeOfCuboid(4, 10, 12), 480)
    test(volumeOfCuboid(2, 4, 6), 48)
    test(volumeOfCuboid(42, 42, 42), 74088)

    // 3. tests for isEven
    test(isEven(10), true)
    test(isEven(14), true)
    test(isEven(123), false)
    test(isEven(0), true)
    test(isEven(1), false)
    test(isEven(2343), false)
    test(isEven(19), false)
    test(isEven(32), true)
    test(isEven(1023123), false)
    test(isEven(80198312), true)

    // 4. tests for isOdd
    test(isOdd(10), false)
    test(isOdd(14), false)
    test(isOdd(123), true)
    test(isOdd(0), false)
    test(isOdd(1), true)
    test(isOdd(2343), true)
    test(isOdd(19), true)
    test(isOdd(32), false)
    test(isOdd(1023123), true)
    test(isOdd(80198312), false)

    // 5. tests for gcd
    test(gcd(123, 245), 1)
    test(gcd(24, 36), 12)
    test(gcd(152, 24), 8)
    test(gcd(2, 32), 2)
    test(gcd(10, 20), 10)
    test(gcd(24, 48), 24)
    test(gcd(1231, 2342), 1)
    test(gcd(141, 141), 141)
    test(gcd(21, 49), 7)
    test(gcd(4827, 5382), 3)

    // 6. tests for lcm
    test(lcm(123, 245), 30135)
    test(lcm(24, 36), 72)
    test(lcm(152, 24), 456)
    test(lcm(2, 32), 32)
    test(lcm(10, 20), 20)
    test(lcm(24, 48), 48)
    test(lcm(1231, 2342), 2883002)
    test(lcm(141, 141), 141)
    test(lcm(21, 49), 147)
    test(lcm(4827, 5382), 8659638)

    // 7. tests for numOfHomework
    test(numOfHomework(CS320(quiz = 4, homework = 3)), 3)
    test(numOfHomework(CS320(quiz = 1, homework = 24)), 24)
    test(numOfHomework(CS320(quiz = 35, homework = 48)), 48)
    test(numOfHomework(CS320(quiz = 1, homework = 10)), 10)
    test(numOfHomework(CS311(homework = 43)), 43)
    test(numOfHomework(CS311(homework = 20)), 20)
    test(numOfHomework(CS330(projects = 52, homework = 10)), 10)
    test(numOfHomework(CS330(projects = 40, homework = 49)), 49)
    test(numOfHomework(CS330(projects = 10, homework = 30)), 30)
    test(numOfHomework(CS330(projects = 24, homework = 123)), 123)

    // 8. tests for hasProjects
    test(hasProjects(CS320(quiz = 3, homework = 9)), false)
    test(hasProjects(CS320(quiz = 24, homework = 1)), false)
    test(hasProjects(CS320(quiz = 48, homework = 0)), false)
    test(hasProjects(CS320(quiz = 10, homework = 20)), false)
    test(hasProjects(CS311(homework = 43)), false)
    test(hasProjects(CS311(homework = 20)), false)
    test(hasProjects(CS330(projects = 10, homework = 0)), true)
    test(hasProjects(CS330(projects = 0, homework = 3)), false)
    test(hasProjects(CS330(projects = 1, homework = 6)), false)
    test(hasProjects(CS330(projects = 123, homework = 3)), true)

    // 9. tests for namePets
    test(namePets(List("dog", "cat", "pig")), List("happy", "smart", "pinky"))
    test(namePets(List("cat", "bat", "bear")), List("smart", "bat", "bear"))
    test(namePets(List("cat", "dog", "dog", "pig")), List("smart", "happy", "happy", "pinky"))
    test(namePets(List("cat", "tiger", "lion")), List("smart", "tiger", "lion"))
    test(namePets(List("cat", "dolphin")), List("smart", "dolphin"))
    test(namePets(List()), List())
    test(namePets(List("a", "b", "c")), List("a", "b", "c"))
    test(namePets(List("dog", "dog", "dog", "dog")), List("happy", "happy", "happy", "happy"))
    test(namePets(List("cat", "cat", "pig", "dog", "dog")), List("smart", "smart", "pinky", "happy", "happy"))
    test(namePets(List("pig", "pig", "pig")), List("pinky", "pinky", "pinky"))

    // 10. tests for giveName
    def split(str: String): List[String] = str.split("").toList
    test(giveName("bear", "pooh")(List("pig", "cat", "bear")), List("pig", "cat", "pooh"))
    test(giveName("pig", "pinky")(List("pig", "cat", "bear")), List("pinky", "cat", "bear"))
    test(giveName("cat", "smart")(List("pig", "cat", "bear")), List("pig", "smart", "bear"))
    test(giveName("a", "b")(split("abckdjeifjwofjghghwizjjwmcmlmnopqrstuvxwcvjwoeijbnrygjwo")), split("bbckdjeifjwofjghghwizjjwmcmlmnopqrstuvxwcvjwoeijbnrygjwo"))
    test(giveName("b", "z")(split("abckdjeifjwofjghghwizjjwmcmlmnopqrstuvxwcvjwoeijbnrygjwo")), split("azckdjeifjwofjghghwizjjwmcmlmnopqrstuvxwcvjwoeijznrygjwo"))
    test(giveName("d", "f")(split("abckdjeifjwofjghghwizjjwmcmlmnopqrstuvxwcvjwoeijbnrygjwo")), split("abckfjeifjwofjghghwizjjwmcmlmnopqrstuvxwcvjwoeijbnrygjwo"))
    test(giveName("w", "f")(split("abckdjeifjwofjghghwizjjwmcmlmnopqrstuvxwcvjwoeijbnrygjwo")), split("abckdjeifjfofjghghfizjjfmcmlmnopqrstuvxfcvjfoeijbnrygjfo"))
    test(giveName("f", "b")(split("abckdjeifjwofjghghwizjjwmcmlmnopqrstuvxwcvjwoeijbnrygjwo")), split("abckdjeibjwobjghghwizjjwmcmlmnopqrstuvxwcvjwoeijbnrygjwo"))
    test(giveName("w", "z")(split("abckdjeifjwofjghghwizjjwmcmlmnopqrstuvxwcvjwoeijbnrygjwo")), split("abckdjeifjzofjghghzizjjzmcmlmnopqrstuvxzcvjzoeijbnrygjzo"))
    test(giveName("y", "f")(split("abckdjeifjwofjghghwizjjwmcmlmnopqrstuvxwcvjwoeijbnrygjwo")), split("abckdjeifjwofjghghwizjjwmcmlmnopqrstuvxwcvjwoeijbnrfgjwo"))
  }


    /* Write your own tests */
}