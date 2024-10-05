import Foundation

struct NegativeAmountError: Error {}
struct NoSuchFileError: Error {}

func change(_ amount: Int) -> Result<[Int:Int], NegativeAmountError> {
    if amount < 0 {
        return .failure(NegativeAmountError())
    }
    var (counts, remaining) = ([Int:Int](), amount)
    for denomination in [25, 10, 5, 1] {
        (counts[denomination], remaining) = 
            remaining.quotientAndRemainder(dividingBy: denomination)
    }
    return .success(counts)
}

// Write your first then lower case function here
func firstThenLowerCase<T>(of list: [T], satisfying predicate: (T) -> Bool) -> String? {
    return list.first(where: predicate).map { String(describing: $0).lowercased() }
}

// Write your say function here
class Say {
    private var _phrase: String

    init(_ word: String = "") {
        self._phrase = word
    }

    func and(_ word: String) -> Say {
        if !_phrase.isEmpty && !word.isEmpty {
            _phrase += " "
        }
        _phrase += word
        return self
    }

    var phrase: String {
        return self._phrase
    }
}

func say(_ word: String = "") -> Say {
    return Say(word)
}

// Write your meaningfulLineCount function here
func meaningfulLineCount(_ filename: String) async -> Result<Int, Error> {
    do {
        let fileURL = URL(fileURLWithPath: filename)
        let contents = try String(contentsOf: fileURL, encoding: .utf8)
        let lines = contents.components(separatedBy: .newlines)
        let count = lines.filter { !$0.trimmingCharacters(in: .whitespaces).isEmpty && !$0.trimmingCharacters(in: .whitespaces).hasPrefix("#") }.count
        return .success(count)
    } catch {
        return .failure(error)
    }
}

// Write your Quaternion struct here
struct Quaternion: Equatable {
    let a: Double
    let b: Double
    let c: Double
    let d: Double

    static let ZERO = Quaternion(a: 0, b: 0, c: 0, d: 0)
    static let I = Quaternion(a: 0, b: 1, c: 0, d: 0)
    static let J = Quaternion(a: 0, b: 0, c: 1, d: 0)
    static let K = Quaternion(a: 0, b: 0, c: 0, d: 1)

    init(a: Double = 0, b: Double = 0, c: Double = 0, d: Double = 0) {
        guard !a.isNaN && !b.isNaN && !c.isNaN && !d.isNaN else {
            fatalError("Coefficients cannot be NaN")
        }
        self.a = a
        self.b = b
        self.c = c
        self.d = d
    }

    var coefficients: [Double] { [a, b, c, d] }

    var conjugate: Quaternion {
        Quaternion(a: a, b: -b, c: -c, d: -d)
    }

    static func + (lhs: Quaternion, rhs: Quaternion) -> Quaternion {
        Quaternion(a: lhs.a + rhs.a, b: lhs.b + rhs.b, c: lhs.c + rhs.c, d: lhs.d + rhs.d)
    }

    static func * (lhs: Quaternion, rhs: Quaternion) -> Quaternion {
        Quaternion(
            a: lhs.a * rhs.a - lhs.b * rhs.b - lhs.c * rhs.c - lhs.d * rhs.d,
            b: lhs.a * rhs.b + lhs.b * rhs.a + lhs.c * rhs.d - lhs.d * rhs.c,
            c: lhs.a * rhs.c - lhs.b * rhs.d + lhs.c * rhs.a + lhs.d * rhs.b,
            d: lhs.a * rhs.d + lhs.b * rhs.c - lhs.c * rhs.b + lhs.d * rhs.a
        )
    }
}

extension Quaternion: CustomStringConvertible {
    var description: String {
        let terms = [
            (a, ""),
            (b, "i"),
            (c, "j"),
            (d, "k")
        ].filter { $0.0 != 0 || $0.1.isEmpty }
        .enumerated()
        .map { index, term in
            let (coeff, unit) = term
            let sign = index == 0 || coeff < 0 ? "" : "+"
            let coeffStr = String(format: "%.1f", coeff).replacingOccurrences(of: ".0", with: "")
            return "\(sign)\(coeffStr)\(unit)"
        }
        return terms.joined().isEmpty ? "0" : terms.joined()
    }
}

// Write your Binary Search Tree enum here
indirect enum BinarySearchTree {
    case empty
    case node(String, BinarySearchTree, BinarySearchTree)

    var size: Int {
        switch self {
        case .empty: return 0
        case .node(_, let left, let right): return 1 + left.size + right.size
        }
    }

    func contains(_ value: String) -> Bool {
        switch self {
        case .empty: return false
        case .node(let v, let left, let right):
            if v == value { return true }
            return value < v ? left.contains(value) : right.contains(value)
        }
    }

    func insert(_ value: String) -> BinarySearchTree {
        switch self {
        case .empty: return .node(value, .empty, .empty)
        case .node(let v, let left, let right):
            if value < v {
                return .node(v, left.insert(value), right)
            } else if value > v {
                return .node(v, left, right.insert(value))
            } else {
                return self
            }
        }
    }
}

extension BinarySearchTree: CustomStringConvertible {
    var description: String {
        switch self {
        case .empty: return "()"
        case .node(let value, let left, let right):
            if case .empty = left, case .empty = right {
                return "(\(value))"
            } else {
                return "(\(left)\(value)\(right))"
            }
        }
    }
}
