// Under the Apache 2.0 license
// (c) Microsoft Corporation 2005-2009.

namespace SeattleQio.Simulator

open System
open System.Globalization

/// Complex number in cartesian form
type Complex =
    { Real: float
      Imaginary: float }

    member x.r = x.Real

    member x.i = x.Imaginary

    override x.ToString() = x.ToString("g")

    member x.ToString(fmt) =
        x.ToString(fmt, CultureInfo.InvariantCulture)

    member x.ToString(fmt, fmtprovider: IFormatProvider) =
        x.r.ToString(fmt, fmtprovider)
        + "r"
        + (if x.i < 0.0 then "-" else "+")
        + (Math.Abs x.i).ToString(fmt, fmtprovider)
        + "i"

/// Complex number
type complex = Complex

/// Operations for complex number in cartesian form
module Complex =
    /// Creates a complex number of a real part a and an imaginary part b
    let mkRect (a, b) = { Real = a; Imaginary = b }

    /// Conjugates a complex number (reverses sign of imaginary part)
    let conjugate (c: complex) = mkRect (c.r, -c.i)

    /// Creates a complex number from its polar form where a is the radius and b is the angle
    let mkPolar (a, b) =
        mkRect (a * Math.Cos(b), a * Math.Sin(b))

    /// Creates a complex number on the unit circle with b being the radius
    let cis b = mkPolar (1.0, b)

    /// Complex number with real part 0 and imaginary part 0
    let zero = mkRect (0., 0.)

    /// Complex number with real part 1 and imaginary part 0
    let one = mkRect (1., 0.)

    /// Complex number with real part 0 and imaginary part 1
    let onei = mkRect (0., 1.)

    /// Evalues magnitude of complex number
    let magnitude (c: complex) = sqrt (c.r * c.r + c.i * c.i)

    /// Evaluates the phase (or the angle) of the complex number in polar form
    let phase (c: complex) = Math.Atan2(c.i, c.r)

    /// Gets the real part of a complex number
    let realPart (c: complex) = c.r

    /// Gets the imaginary part of a complex number
    let imagPart (c: complex) = c.i

    /// Evaluates the absolute value of a complex number
    let abs (a: complex) = sqrt (a.r ** 2.0 + a.i ** 2.0)

    /// Adds two complex numbers
    let add (a: complex) (b: complex) = mkRect (a.r + b.r, a.i + b.i)

    /// Substracts complex number b from complex number a
    let sub (a: complex) (b: complex) = mkRect (a.r - b.r, a.i - b.i)

    /// Multiplies two complex numbers
    let mul (a: complex) (b: complex) =
        mkRect (a.r * b.r - a.i * b.i, a.i * b.r + b.i * a.r)

    /// Divides complex number x by complex number y
    let div (x: complex) (y: complex) =
        let a = x.r
        let b = x.i
        let c = y.r
        let d = y.i
        // (a + ib) / (c + id) = (ac + bd + i(bc - ad)) / (c2 + d2)
        let q = c * c + d * d
        mkRect ((a * c + b * d) / q, (b * c - a * d) / q)

    /// Reverses sign of the real and the imaginary part of a complex number
    let neg (a: complex) = mkRect (-a.r, -a.i)

    /// Multiplies complex number b by scalar a
    let smul (a: float) (b: complex) = mkRect (a * b.r, a * b.i)

    /// Multiplies complex number a by scalar b
    let muls (a: complex) (b: float) = mkRect (a.r * b, a.i * b)

    let of_string s = mkRect (Double.Parse s, 0.0)

    /// Rotates a complex number 90° counterclockwise and multiplies by scalar k
    let iscale k (x: complex) =
        // ik.(r + i.th) = -k.th + i.k.r
        mkRect (-k * x.i, k * x.r)

    // LogN : 'a * 'a -> 'a
    // Asin : 'a -> 'a
    // Acos : 'a -> 'a
    // Atan : 'a -> 'a
    // Atan2 : 'a * 'a -> 'a
    // Sinh : 'a -> 'a
    // Cosh : 'a -> 'a
    // Tanh : 'a -> 'a

    /// Pi as complex number
    let pi = mkRect (Math.PI, 0.0)

    /// Evaluates the exponent of the complex number to base e
    let exp (x: complex) =
        // exp(r+it) = exp(r).(cos(t)+i.sin(t)) - De Moivre Theorem
        smul (exp (x.r)) (mkRect (cos (x.i), sin (x.i)))

    /// Evaluates natural logarithm of complex number
    let log x =
        // x = mag.e^(i.th) = e^ln(mag).e^(i.th) = e^(ln(mag) + i.th)
        mkRect (log (magnitude (x)), phase (x))

    /// Evaluates square root of complex number
    let sqrt x =
        mkPolar (sqrt (magnitude x), phase x / 2.0)

    /// Evaluates the cosine of a complex number
    let cos x =
        // cos(x) = (exp(i.x) + exp(-i.x))/2
        smul 0.5 (add (exp (iscale 1.0 x)) (exp (iscale -1.0 x)))

    /// Evaluates the sine of a complex number
    let sin x =
        // sin(x) = (exp(i.x) - exp(-i.x))/2 . (-i)
        smul 0.5 (sub (exp (iscale 1.0 x)) (exp (iscale -1.0 x)))
        |> iscale (-1.0)

    /// Evaluates the tangent of a complex number
    let tan x =
        // tan(x) = (exp(i.x) - exp(-i.x)) . (-i) / (exp(i.x) + exp(-i.x))
        //        = (exp(2i.x) - 1.0)      . (-i) / (exp(2i.x) + 1.0)
        let exp2ix = exp (iscale 2.0 x)

        div (sub exp2ix one) (add exp2ix one)
        |> iscale -1.0

    /// Creates a complex number of a real number (imaginary part 0)
    let ofReal r = mkRect (r, 0.)

type Complex with
    static member Create(a, b) = Complex.mkRect (a, b)
    static member CreatePolar(a, b) = Complex.mkPolar (a, b)
    member x.Magnitude = Complex.magnitude x
    member x.Phase = Complex.phase x
    member x.RealPart = x.r
    member x.ImaginaryPart = x.i
    member x.Conjugate = Complex.conjugate x

    static member Sin(x) = Complex.sin (x)
    static member Cos(x) = Complex.cos (x)
    static member Abs(x) = Complex.abs (x)
    static member Tan(x) = Complex.tan (x)
    static member Log(x) = Complex.log (x)
    static member Exp(x) = Complex.exp (x)
    static member Sqrt(x) = Complex.sqrt (x)

    static member Zero = Complex.zero
    static member One = Complex.one
    static member OneI = Complex.onei
    static member (+)(a, b) = Complex.add a b
    static member (-)(a, b) = Complex.sub a b
    static member (*)(a, b) = Complex.mul a b
    static member (/)(a, b) = Complex.div a b
    static member (~-) a = Complex.neg a
    static member (*)(a, b) = Complex.smul a b
    static member (*)(a, b) = Complex.muls a b

[<AutoOpen>]
module ComplexTopLevelOperators =
    let complex x y = Complex.mkRect (x, y)
