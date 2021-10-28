package scalashop

import java.util.concurrent._
import scala.collection._
import org.junit._
import org.junit.Assert.assertEquals

class BlurSuite {

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)

  /**
    * ======= LOG OF FAILED TESTS ========
    *
    * [Test Description] HorizontalBoxBlur.parBlur with 32 tasks should modify each pixel of the destination 32x32 image exactly once (2pts)(scalashop.BlurSuite)
    * [Observed Error] assertion failed: (access count at 0, 31 is not 1)
    *
    * [Test Description] VerticalBoxBlur.parBlur with 32 tasks should execute 32 parallel tasks for a 32x64 image, each blurring one strip (3pts)(scalashop.BlurSuite)
    * [Observed Error] assertion failed: A parallel task did not blur any new pixels.
    *
    * [Test Description] HorizontalBoxBlur.parBlur with radius 1 and 1 task should correctly blur the entire 3x3 image(scalashop.BlurSuite)
    * [Observed Error] (destination(0, 2) should be 5) expected:<5> but was:<0>
    *
    * [Test Description] HorizontalBoxBlur.parBlur with radius 1 and 4 tasks should correctly blur the entire 3x3 image(scalashop.BlurSuite)
    * [Observed Error] (destination(0, 2) should be 5) expected:<5> but was:<0>
    *
    * [Test Description] VerticalBoxBlur.parBlur with radius 1 and 1 task should correctly blur the entire 3x3 image(scalashop.BlurSuite)
    * [Observed Error] (destination(2, 0) should be 3) expected:<3> but was:<0>
    *
    * [Test Description] VerticalBoxBlur.parBlur with radius 1 and 4 tasks should correctly blur the entire 3x3 image(scalashop.BlurSuite)
    * [Observed Error] (destination(2, 0) should be 3) expected:<3> but was:<0>
    *
    * [Test Description] VerticalBoxBlur.parBlur with 32 tasks should modify each pixel of the destination 32x32 image exactly once (2pts)(scalashop.BlurSuite)
    * [Observed Error] assertion failed: (access count at 31, 0 is not 1)
    *
    * [Test Description] HorizontalBoxBlur.parBlur should not forget the last strip(scalashop.BlurSuite)
    * [Observed Error] (destination(2, 2) should be 6) expected:<6> but was:<0>
    *
    * [Test Description] HorizontalBoxBlur.parBlur with 32 tasks should execute 32 parallel tasks for a 64x32 image, each blurring one strip (3pts)(scalashop.BlurSuite)
    * [Observed Error] assertion failed: A parallel task did not blur any new pixels.
    *
    * [Test Description] VerticalBoxBlur.parBlur should not forget the last strip(scalashop.BlurSuite)
    * [Observed Error] (destination(2, 2) should be 6) expected:<6> but was:<0>
    *
    * [Test Description] VerticalBoxBlur.parBlur with radius 1 and 32 tasks should correctly blur the entire 3x3 image(scalashop.BlurSuite)
    * [Observed Error] (destination(2, 0) should be 24) expected:<24> but was:<0>
    *
    * [Test Description] HorizontalBoxBlur.parBlur with radius 1 and 32 tasks should correctly blur the entire 3x3 image(scalashop.BlurSuite)
    * [Observed Error] (destination(0, 2) should be 22) expected:<22> but was:<0>
    */
}
