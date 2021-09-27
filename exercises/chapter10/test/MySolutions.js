"use strict";

// Note to reader: Add your solutions to this file
exports.volumeFn = function (w, h, d) { return w * h * d }

exports.volumeArrow = w => h => d => w * h * d

exports.cumulativeSumsComplex = ary => {
    // array of {real, imag}
    return ary.reduce((acc, a) => {
        if (acc.length == 0) {
            return [a]
        } else {
            const prev = acc[acc.length - 1]
            return [...acc,
            {
                real: prev.real + a.real,
                imag: prev.imag + a.imag,
            }]
        }
    }, [])
}

// Quadratic Formula
// x = (-b/ 2*a ) + sqrt(b^2 -4*a*c)/ 2*a
// x = (-b/ 2*a ) - sqrt(b^2 -4*a*c)/ 2*a

exports.quadraticRootsPair = pair => poly => {
    // pair constructor. 
    // returns pair Complex
    // poly: {a, b, c}
    const p1 = (- poly.b / (2.0 * poly.a))
    const p2_ = poly.b * poly.b - 4 * poly.a * poly.c
    if (p2_ >= 0) {
        return pair({ real: p1 + Math.sqrt(p2_) / (2.0 * poly.a), imag: 0.0 })(
            { real: p1 - Math.sqrt(p2_) / (2.0 * poly.a), imag: 0.0 })
    } else {
        return pair({ real: p1, imag: + Math.sqrt(-p2_) / (2.0 * poly.a) })(
            { real: p1, imag: - Math.sqrt(-p2_) / (2.0 * poly.a) })
    }
}

exports.toMaybeImpl = just => nothing => v => {
    if (v === undefined) {
        return nothing
    } else {
        return just(v)
    }
}

// valuesOfMap
exports.valuesOfMapImpl = m => {
    const inMap = new Map(m)
    return Array.from(new Set(inMap.values()))
}