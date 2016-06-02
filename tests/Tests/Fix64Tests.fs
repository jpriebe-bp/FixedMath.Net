module Fix64Tests
open System
open Xunit
open FixMath.NET

let testCases = [|
    // Small numbers
    0L; 1L; 2L; 3L; 4L; 5L; 6L; 7L; 8L; 9L; 10L;
    -1L; -2L; -3L; -4L; -5L; -6L; -7L; -8L; -9L; -10L;

    // Integer numbers
    0x100000000L; -0x100000000L; 0x200000000L; -0x200000000L; 0x300000000L; -0x300000000L;
    0x400000000L; -0x400000000L; 0x500000000L; -0x500000000L; 0x600000000L; -0x600000000L;
 
    // Fractions (1/2L; 1/4L; 1/8)
    0x80000000L; -0x80000000L; 0x40000000L; -0x40000000L; 0x20000000L; -0x20000000L;
  
    // Problematic carry
    0xFFFFFFFFL; -0xFFFFFFFFL; 0x1FFFFFFFFL; -0x1FFFFFFFFL; 0x3FFFFFFFFL; -0x3FFFFFFFFL;
  
    // Smallest and largest values
    Int64.MaxValue; Int64.MinValue;
  
    // Large random numbers
    6791302811978701836L; -8192141831180282065L; 6222617001063736300L; -7871200276881732034L;
    8249382838880205112L; -7679310892959748444L; 7708113189940799513L; -5281862979887936768L;
    8220231180772321456L; -5204203381295869580L; 6860614387764479339L; -9080626825133349457L;
    6658610233456189347L; -6558014273345705245L; 6700571222183426493L;
  
    // Small random numbers
    -436730658L; -2259913246L; 329347474L; 2565801981L; 3398143698L; 137497017L; 1060347500L;
    -3457686027L; 1923669753L; 2891618613L; 2418874813L; 2899594950L; 2265950765L; -1962365447L;
    3077934393L;

    // Tiny random numbers
    - 171L;
    -359L; 491L; 844L; 158L; -413L; -422L; -737L; -575L; -330L;
    -376L; 435L; -311L; 116L; 715L; -1024L; -487L; 59L; 724L; 993L;
|]

let int32max = int64 Int32.MaxValue
let int32min = int64 Int32.MinValue
let int64max = Int64.MaxValue
let int64min = Int64.MinValue

let inline areEqualWithinPrecision value1 value2 precision =
    Assert.True(abs(value2 - value1) < precision)

[<Fact>]
let precision() =
    Assert.Equal(Fix64.Precision, 0.00000000023283064365386962890625m)

    
[<Fact>]
let intToFix64AndBack() =
    let sources = [| Int32.MinValue; -1; 0; 1; Int32.MaxValue; |]
    let expecteds = [| Int32.MinValue; -1; 0; 1; Int32.MaxValue; |]
    for i = 0 to sources.Length - 1 do 
        let expected = expecteds.[i]
        let f = Fix64.op_Explicit sources.[i] 
        let actual = f |> int32
        Assert.Equal(expected, actual)

    Assert.Equal(Int32.MaxValue, Fix64.MaxValue |> int32)
    Assert.Equal(Int32.MinValue, Fix64.MinValue |> int32)

[<Fact>]
let doubleToFix64AndBack() =
    [| -Math.PI; -Math.E; Math.PI; Math.E |]
    |> Array.iter(fun s -> 
        areEqualWithinPrecision s ((Fix64.op_Explicit s) |> float) (Fix64.Precision |> float))

    [| (float Int32.MinValue);  -1.0; -0.0; 0.0; 1.0; (float Int32.MaxValue) |] 
    |> Array.iter(fun s -> Assert.Equal(s, (Fix64.op_Explicit s) |> float))

[<Fact>]
let decimalToFix64AndBack() =
    [| -Math.PI ; -Math.E; Math.PI; Math.E |]
    |> Array.map decimal
    |> Array.iter(fun s -> 
        areEqualWithinPrecision s ((Fix64.op_Explicit s) |> decimal) Fix64.Precision)

    [| (decimal Int32.MinValue); -1.0M; 0.0M; 1.0M; (decimal Int32.MaxValue) |] 
    |> Array.iter(fun s -> Assert.Equal(s, (Fix64.op_Explicit s) |> decimal))

[<Fact>]
let addition() =
    let terms1 = [| Fix64.MinValue; Fix64.op_Explicit -1; Fix64.Zero; Fix64.One; Fix64.MaxValue |]
    let terms2 = [| Fix64.op_Explicit -1; Fix64.op_Explicit 2; Fix64.op_Explicit -1.5M; Fix64.op_Explicit -2; Fix64.One |]
    let expecteds = [| Fix64.MinValue; Fix64.One; Fix64.op_Explicit -1.5M; Fix64.op_Explicit -1; Fix64.MaxValue |]

    for i = 0 to terms1.Length - 1 do
        let actual = terms1.[i] + terms2.[i]
        let expected = expecteds.[i]
        Assert.Equal(expected, actual)

[<Fact>]
let noOverflowMultiplication() =
    let terms1 = [| 0M; 1M; -1M; 5M; -5M; 0.5M; -0.5M; -1.0M |] |> Array.map Fix64.op_Explicit
    let terms2 = [| 16M; 16M; 16M; 16M; 16M; 16M; 16M; -1.0M |] |> Array.map Fix64.op_Explicit
    let expecteds = [| 0; 16; -16; 80; -80; 8; -8; 1 |] |> Array.map Fix64.op_Explicit

    for i = 0 to terms1.Length - 1 do
        let actual = terms1.[i] * terms2.[i]
        let expected = expecteds.[i]
        Assert.Equal(expected, actual)

[<Fact>]
let multiplication() =
    let terms = testCases |> Array.map Fix64.FromRaw
    for i = 0 to terms.Length - 1 do
        for j = 0 to terms.Length - 1 do
            let x = terms.[i]
            let y = terms.[j]

            let expected = (decimal x) * (decimal y)
            let expected = if expected > (decimal Fix64.MaxValue) then (decimal Fix64.MaxValue)
                           elif expected < (decimal Fix64.MinValue) then (decimal Fix64.MinValue)
                           else expected

            let actual = x * y
            areEqualWithinPrecision expected (decimal actual) Fix64.Precision

[<Fact>]
let division() =
    let terms = testCases |> Array.map Fix64.FromRaw
    for i = 0 to terms.Length - 1 do
        for j = 0 to terms.Length - 1 do
            let x = terms.[i]
            let y = terms.[j]

            if y = Fix64.Zero then
                Assert.Throws<DivideByZeroException>(fun () -> (x / y) |> ignore) |> ignore
            else
                let expected = (decimal x) / (decimal y)
                let expected = if expected > (decimal Fix64.MaxValue) then (decimal Fix64.MaxValue)
                               elif expected < (decimal Fix64.MinValue) then (decimal Fix64.MinValue)
                               else expected            
                let actual = x / y
                areEqualWithinPrecision expected (decimal actual) Fix64.Precision

[<Fact>]
let sign() =
    let sources = [| Fix64.MinValue; -Fix64.One; Fix64.Zero; Fix64.One; Fix64.MaxValue |]
    let expecteds = [| -1; -1; 0; 1; 1 |]
    
    for i = 0 to sources.Length - 1 do
        let actual = Fix64.Sign sources.[i]
        let expected = expecteds.[i]
        Assert.Equal(expected, actual)

// TODO test unary minus
[<Fact>]
let abs() =
    Assert.Equal(Fix64.MaxValue, abs(Fix64.MinValue))
    let sources = [| -Fix64.One; Fix64.Zero; Fix64.One; Fix64.MaxValue |]
    let expecteds = [| Fix64.One; Fix64.Zero; Fix64.One; Fix64.MaxValue |]
    for i = 0 to sources.Length - 1 do
        let actual = abs sources.[i]
        let expected = expecteds.[i]
        Assert.Equal(expected, actual)

[<Fact>]
let fastAbs() =
    Assert.Equal(Fix64.MinValue, Fix64.FastAbs(Fix64.MinValue))
    let sources = [| -Fix64.One; Fix64.Zero; Fix64.One; Fix64.MaxValue |]
    let expecteds = [| Fix64.One; Fix64.Zero; Fix64.One; Fix64.MaxValue |]
    for i = 0 to sources.Length - 1 do
        let actual = Fix64.FastAbs sources.[i]
        let expected = expecteds.[i]
        Assert.Equal(expected, actual)


//[<Fact>]
//let floor() {
//    var sources = new[] { -5.1m, -1, 0, 1, 5.1m };
//    var expecteds = new[] { -6m, -1, 0, 1, 5m };
//    for (int i = 0; i < sources.Length; ++i) {
//        var actual = (decimal)Fix64.Floor((Fix64)sources[i]);
//        var expected = expecteds[i];
//        Assert.AreEqual(expected, actual);
//    }
//}
//
//        [Test]
//        public void Ceiling() {
//            var sources = new[] { -5.1m, -1, 0, 1, 5.1m };
//            var expecteds = new[] { -5m, -1, 0, 1, 6m };
//            for (int i = 0; i < sources.Length; ++i) {
//                var actual = (decimal)Fix64.Ceiling((Fix64)sources[i]);
//                var expected = expecteds[i];
//                Assert.AreEqual(expected, actual);
//            }
//
//            Assert.AreEqual(Fix64.MaxValue, Fix64.Ceiling(Fix64.MaxValue));
//        }
//
//        [Test]
//        public void Round() {
//            var sources = new[] { -5.5m, -5.1m, -4.5m, -4.4m, -1, 0, 1, 4.5m, 4.6m, 5.4m, 5.5m };
//            var expecteds = new[] { -6m, -5m, -4m, -4m, -1, 0, 1, 4m, 5m, 5m, 6m };
//            for (int i = 0; i < sources.Length; ++i) {
//                var actual = (decimal)Fix64.Round((Fix64)sources[i]);
//                var expected = expecteds[i];
//                Assert.AreEqual(expected, actual);
//            }
//            Assert.AreEqual(Fix64.MaxValue, Fix64.Round(Fix64.MaxValue));
//        }
//
//
//        [Test]
//        public void Sqrt() {
//            for (int i = 0; i < m_testCases.Length; ++i) {
//                var f = Fix64.FromRaw(m_testCases[i]);
//                if (Fix64.Sign(f) < 0) {
//                    Assert.Throws<ArgumentOutOfRangeException>(() => Fix64.Sqrt(f));
//                }
//                else {
//                    var expected = Math.Sqrt((double)f);
//                    var actual = (double)Fix64.Sqrt(f);
//                    var delta = (decimal)Math.Abs(expected - actual);
//                    Assert.LessOrEqual(delta, Fix64.Precision);
//                }
//            }
//        }
//
//        [Test]
//        public void Modulus() {
//            var deltas = new List<decimal>();
//            foreach (var operand1 in m_testCases) {
//                foreach (var operand2 in m_testCases) {
//                    var f1 = Fix64.FromRaw(operand1);
//                    var f2 = Fix64.FromRaw(operand2);
//
//                    if (operand2 == 0) {
//                        Assert.Throws<DivideByZeroException>(() => Ignore(f1 / f2));
//                    }
//                    else {
//                        var d1 = (decimal)f1;
//                        var d2 = (decimal)f2;
//                        var actual = (decimal)(f1 % f2);
//                        var expected = d1 % d2;
//                        var delta = Math.Abs(expected - actual);
//                        deltas.Add(delta);
//                        Assert.LessOrEqual(delta, 60 * Fix64.Precision, string.Format("{0} % {1} = expected {2} but got {3}", f1, f2, expected, actual));
//                    }
//                }
//            }
//            Console.WriteLine("Max error: {0} ({1} times precision)", deltas.Max(), deltas.Max() / Fix64.Precision);
//            Console.WriteLine("Average precision: {0} ({1} times precision)", deltas.Average(), deltas.Average() / Fix64.Precision);
//            Console.WriteLine("failed: {0}%", deltas.Count(d => d > Fix64.Precision) * 100.0 / deltas.Count);
//        }
//
//        [Test]
//        public void SinBenchmark() {
//            var deltas = new List<double>();
//
//            var swf = new Stopwatch();
//            var swd = new Stopwatch();
//
//            // Restricting the range to from 0 to Pi/2
//            for (var angle = 0.0; angle <= 2 * Math.PI ; angle += 0.000004) {
//                var f = (Fix64)angle;
//                swf.Start();
//                var actualF = Fix64.Sin(f);
//                swf.Stop();
//                var actual = (double)actualF;
//                swd.Start();
//                var expectedD = Math.Sin(angle);
//                swd.Stop();
//                var expected = (double)expectedD;
//                var delta = Math.Abs(expected - actual);
//                deltas.Add(delta);
//            }
//            Console.WriteLine("Max error: {0} ({1} times precision)", deltas.Max(), deltas.Max() / (double)Fix64.Precision);
//            Console.WriteLine("Average precision: {0} ({1} times precision)", deltas.Average(), deltas.Average() / (double)Fix64.Precision);
//            Console.WriteLine("Fix64.Sin time = {0}ms, Math.Sin time = {1}ms", swf.ElapsedMilliseconds, swd.ElapsedMilliseconds);
//        }
//
//        [Test]
//        public void Sin() {
//            Assert.True(Fix64.Sin(Fix64.Zero) == Fix64.Zero);
//
//            Assert.True(Fix64.Sin(Fix64.PiOver2) == Fix64.One);
//            Assert.True(Fix64.Sin(Fix64.Pi) == Fix64.Zero);
//            Assert.True(Fix64.Sin(Fix64.Pi + Fix64.PiOver2) == -Fix64.One);
//            Assert.True(Fix64.Sin(Fix64.PiTimes2) == Fix64.Zero);
//
//            Assert.True(Fix64.Sin(-Fix64.PiOver2) == -Fix64.One);
//            Assert.True(Fix64.Sin(-Fix64.Pi) == Fix64.Zero);
//            Assert.True(Fix64.Sin(-Fix64.Pi - Fix64.PiOver2) == Fix64.One);
//            Assert.True(Fix64.Sin(-Fix64.PiTimes2) == Fix64.Zero);
//
//
//            for (double angle = -2 * Math.PI; angle <= 2 * Math.PI; angle += 0.0001) {
//                var f = (Fix64)angle;
//                var actualF = Fix64.Sin(f);
//                var expected = (decimal)Math.Sin(angle);
//                var delta = Math.Abs(expected - (decimal)actualF);
//                Assert.LessOrEqual(delta, 3 * Fix64.Precision, string.Format("Sin({0}): expected {1} but got {2}", angle, expected, actualF));
//            }
//
//            foreach (var val in m_testCases) {
//                var f = Fix64.FromRaw(val);
//                var actualF = Fix64.Sin(f);
//                var expected = (decimal)Math.Sin((double)f);
//                var delta = Math.Abs(expected - (decimal)actualF);
//                Assert.LessOrEqual(delta, 0.003, string.Format("Sin({0}): expected {1} but got {2}", f, expected, actualF));
//            }
//
//            Console.WriteLine("Max delta = {0}", m_testCases.Max(val => {
//                var f = Fix64.FromRaw(val);
//                var actualF = Fix64.Sin(f);
//                var expected = (decimal)Math.Sin((double)f);
//                return Math.Abs(expected - (decimal)actualF);
//            }));
//        }
//
//        [Test]
//        public void FastSin() {
//            for (double angle = -2 * Math.PI; angle <= 2 * Math.PI; angle += 0.0001) {
//                var f = (Fix64)angle;
//                var actualF = Fix64.FastSin(f);
//                var expected = (decimal)Math.Sin(angle);
//                var delta = Math.Abs(expected - (decimal)actualF);
//                Assert.LessOrEqual(delta, 50000 * Fix64.Precision, string.Format("Sin({0}): expected {1} but got {2}", angle, expected, actualF));
//            }
//
//            foreach (var val in m_testCases) {
//                var f = Fix64.FromRaw(val);
//                var actualF = Fix64.FastSin(f);
//                var expected = (decimal)Math.Sin((double)f);
//                var delta = Math.Abs(expected - (decimal)actualF);
//                Assert.LessOrEqual(delta, 0.01, string.Format("Sin({0}): expected {1} but got {2}", f, expected, actualF));
//            }
//        }
//
//        [Test]
//        public void Cos() {
//            Assert.True(Fix64.Cos(Fix64.Zero) == Fix64.One);
//                              
//            Assert.True(Fix64.Cos(Fix64.PiOver2) == Fix64.Zero);
//            Assert.True(Fix64.Cos(Fix64.Pi) == -Fix64.One);
//            Assert.True(Fix64.Cos(Fix64.Pi + Fix64.PiOver2) == Fix64.Zero);
//            Assert.True(Fix64.Cos(Fix64.PiTimes2) == Fix64.One);
//                              
//            Assert.True(Fix64.Cos(-Fix64.PiOver2) == -Fix64.Zero);
//            Assert.True(Fix64.Cos(-Fix64.Pi) == -Fix64.One);
//            Assert.True(Fix64.Cos(-Fix64.Pi - Fix64.PiOver2) == Fix64.Zero);
//            Assert.True(Fix64.Cos(-Fix64.PiTimes2) == Fix64.One);
//
//
//            for (double angle = -2 * Math.PI; angle <= 2 * Math.PI; angle += 0.0001) {
//                var f = (Fix64)angle;
//                var actualF = Fix64.Cos(f);
//                var expected = (decimal)Math.Cos(angle);
//                var delta = Math.Abs(expected - (decimal)actualF);
//                Assert.LessOrEqual(delta, 3 * Fix64.Precision, string.Format("Cos({0}): expected {1} but got {2}", angle, expected, actualF));
//            }
//
//            foreach (var val in m_testCases) {
//                var f = Fix64.FromRaw(val);
//                var actualF = Fix64.Cos(f);
//                var expected = (decimal)Math.Cos((double)f);
//                var delta = Math.Abs(expected - (decimal)actualF);
//                Assert.LessOrEqual(delta, 0.004, string.Format("Cos({0}): expected {1} but got {2}", f, expected, actualF));
//            }
//        }
//
//        [Test]
//        public void FastCos() {
//            for (double angle = -2 * Math.PI; angle <= 2 * Math.PI; angle += 0.0001) {
//                var f = (Fix64)angle;
//                var actualF = Fix64.FastCos(f);
//                var expected = (decimal)Math.Cos(angle);
//                var delta = Math.Abs(expected - (decimal)actualF);
//                Assert.LessOrEqual(delta, 50000 * Fix64.Precision, string.Format("Cos({0}): expected {1} but got {2}", angle, expected, actualF));
//            }
//
//            foreach (var val in m_testCases) {
//                var f = Fix64.FromRaw(val);
//                var actualF = Fix64.FastCos(f);
//                var expected = (decimal)Math.Cos((double)f);
//                var delta = Math.Abs(expected - (decimal)actualF);
//                Assert.LessOrEqual(delta, 0.01, string.Format("Cos({0}): expected {1} but got {2}", f, expected, actualF));
//            }
//        }
//
//        [Test]
//        public void Tan() {
//            Assert.True(Fix64.Tan(Fix64.Zero) == Fix64.Zero);
//            Assert.True(Fix64.Tan(Fix64.Pi) == Fix64.Zero);
//            Assert.True(Fix64.Tan(-Fix64.Pi) == Fix64.Zero);
//
//            Assert.True(Fix64.Tan(Fix64.PiOver2 - (Fix64)0.001) > Fix64.Zero);
//            Assert.True(Fix64.Tan(Fix64.PiOver2 + (Fix64)0.001) < Fix64.Zero);
//            Assert.True(Fix64.Tan(-Fix64.PiOver2 - (Fix64)0.001) > Fix64.Zero);
//            Assert.True(Fix64.Tan(-Fix64.PiOver2 + (Fix64)0.001) < Fix64.Zero);
//
//            for (double angle = 0;/*-2 * Math.PI;*/ angle <= 2 * Math.PI; angle += 0.0001) {
//                var f = (Fix64)angle;
//                var actualF = Fix64.Tan(f);
//                var expected = (decimal)Math.Tan(angle);
//                Assert.AreEqual(actualF > Fix64.Zero, expected > 0, string.Format("Signs differ for {0}", angle));
//                //TODO figure out a real way to test this function
//            }
//
//            //foreach (var val in m_testCases) {
//            //    var f = (Fix64)val;
//            //    var actualF = Fix64.Tan(f);
//            //    var expected = (decimal)Math.Tan((double)f);
//            //    var delta = Math.Abs(expected - (decimal)actualF);
//            //    Assert.LessOrEqual(delta, 0.01, string.Format("Tan({0}): expected {1} but got {2}", f, expected, actualF));
//            //}
//        }
//
//        [Test]
//        public void Atan2() {
//            var deltas = new List<decimal>();
//            // Identities
//            Assert.AreEqual(Fix64.Atan2(Fix64.Zero, -Fix64.One), Fix64.Pi);
//            Assert.AreEqual(Fix64.Atan2(Fix64.Zero, Fix64.Zero), Fix64.Zero);
//            Assert.AreEqual(Fix64.Atan2(Fix64.Zero, Fix64.One), Fix64.Zero);
//            Assert.AreEqual(Fix64.Atan2(Fix64.One, Fix64.Zero), Fix64.PiOver2);
//            Assert.AreEqual(Fix64.Atan2(-Fix64.One, Fix64.Zero), -Fix64.PiOver2);
//
//            // Precision
//            for (var y = -1.0; y < 1.0; y += 0.01) {
//                for (var x = -1.0; x < 1.0; x += 0.01) {
//                    var yf = (Fix64)y;
//                    var xf = (Fix64)x;
//                    var actual = (decimal)Fix64.Atan2(yf, xf);
//                    var expected = (decimal)Math.Atan2((double)yf, (double)xf);
//                    var delta = Math.Abs(actual - expected);
//                    deltas.Add(delta);
//                    Assert.LessOrEqual(delta, 0.005, string.Format("Precision: Atan2({0}, {1}): expected {2} but got {3}", yf, xf, expected, actual));
//                }
//            }
//
//            // Scalability and edge cases
//            foreach (var y in m_testCases) {
//                foreach (var x in m_testCases) {
//                    var yf = (Fix64)y;
//                    var xf = (Fix64)x;
//                    var actual = (decimal)Fix64.Atan2(yf, xf);
//                    var expected = (decimal)Math.Atan2((double)yf, (double)xf);
//                    var delta = Math.Abs(actual - expected);
//                    deltas.Add(delta);
//                    Assert.LessOrEqual(delta, 0.005, string.Format("Scalability: Atan2({0}, {1}): expected {2} but got {3}", yf, xf, expected, actual));
//                }
//            }
//            Console.WriteLine("Max error: {0} ({1} times precision)", deltas.Max(), deltas.Max() / Fix64.Precision);
//            Console.WriteLine("Average precision: {0} ({1} times precision)", deltas.Average(), deltas.Average() / Fix64.Precision);
//        }
//
//
//        //[Test]
//        public void Atan2Benchmark() {
//            var deltas = new List<decimal>();
//
//            var swf = new Stopwatch();
//            var swd = new Stopwatch();
//
//            foreach (var y in m_testCases) {
//                foreach (var x in m_testCases) {
//                    for (int k = 0; k < 1000; ++k) {
//                        var yf = (Fix64)y;
//                        var xf = (Fix64)x;
//                        swf.Start();
//                        var actualF = Fix64.Atan2(yf, xf);
//                        swf.Stop();
//                        swd.Start();
//                        var expected = Math.Atan2((double)yf, (double)xf);
//                        swd.Stop();
//                        deltas.Add(Math.Abs((decimal)actualF - (decimal)expected));
//                    }
//                }
//            }
//            Console.WriteLine("Max error: {0} ({1} times precision)", deltas.Max(), deltas.Max() / Fix64.Precision);
//            Console.WriteLine("Average precision: {0} ({1} times precision)", deltas.Average(), deltas.Average() / Fix64.Precision);
//            Console.WriteLine("Fix64.Atan2 time = {0}ms, Math.Atan2 time = {1}ms", swf.ElapsedMilliseconds, swd.ElapsedMilliseconds);
//        }
//
//        [Test]
//        public void Negation() {
//            foreach (var operand1 in m_testCases) {
//                var f = Fix64.FromRaw(operand1);
//                if (f == Fix64.MinValue) {
//                    Assert.AreEqual(-f, Fix64.MaxValue);
//                }
//                else {
//                    var expected = -((decimal)f);
//                    var actual = (decimal)(-f);
//                    Assert.AreEqual(expected, actual);
//                }
//            }
//        }
//
//        [Test]
//        public void Equals() {
//            foreach (var op1 in m_testCases) {
//                foreach (var op2 in m_testCases) {
//                    var d1 = (decimal)op1;
//                    var d2 = (decimal)op2;
//                    Assert.True(op1.Equals(op2) == d1.Equals(d2));
//                }
//            }
//        }
//
//        [Test]
//        public void EqualityAndInequalityOperators() {
//            var sources = m_testCases.Select(Fix64.FromRaw).ToList();
//            foreach (var op1 in sources) {
//                foreach (var op2 in sources) {
//                    var d1 = (double)op1;
//                    var d2 = (double)op2;
//                    Assert.True((op1 == op2) == (d1 == d2));
//                    Assert.True((op1 != op2) == (d1 != d2));
//                    Assert.False((op1 == op2) && (op1 != op2));
//                }
//            }
//        }
//
//        [Test]
//        public void CompareTo() {
//            var nums = m_testCases.Select(Fix64.FromRaw).ToArray();
//            var numsDecimal = nums.Select(t => (decimal)t).ToArray();
//            Array.Sort(nums);
//            Array.Sort(numsDecimal);
//            Assert.True(nums.Select(t => (decimal)t).SequenceEqual(numsDecimal));
//        }
//
//        //[Test]
//        public void GenerateLuts() {
//            Fix64.GenerateSinLut();
//            Fix64.GenerateTanLut();
//        }
//    }
//}
