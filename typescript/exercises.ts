import { open } from "node:fs/promises"

export function change(amount: bigint): Map<bigint, bigint> {
  if (amount < 0) {
    throw new RangeError("Amount cannot be negative")
  }
  let counts: Map<bigint, bigint> = new Map()
  let remaining = amount
  for (const denomination of [25n, 10n, 5n, 1n]) {
    counts.set(denomination, remaining / denomination)
    remaining %= denomination
  }
  return counts
}

// Write your first then apply function here
export function firstThenApply<T, U>(
  arr: T[],
  predicate: (x: T) => boolean,
  transform: (x: T) => U
): U | undefined {
  const found = arr.find(predicate)
  return found === undefined ? undefined : transform(found)
}

// Write your powers generator here
export function* powersGenerator(base: bigint): Generator<bigint> {
  let power: bigint = 1n
  while (true) {
    yield power
    power *= base
  }
}

// Write your line count function here
export async function meaningfulLineCount(filename: string): Promise<number> {
  try {
    const file = await open(filename)
    let count = 0
    
    for await (const line of file.readLines()) {
      const trimmedLine = line.trim()
      if (trimmedLine && !trimmedLine.startsWith('#')) {
        count++
      }
    }
    
    return count
  } catch (error: any) {
    throw new Error(`Error reading file: ${error.message}`)
  }
}

// Write your shape type and associated functions here
export type Shape = 
  | { kind: "Sphere"; radius: number }
  | { kind: "Box"; width: number; length: number; depth: number }

export function volume(shape: Shape): number {
  switch (shape.kind) {
    case "Sphere":
      return (4/3) * Math.PI * Math.pow(shape.radius, 3)
    case "Box":
      return shape.width * shape.length * shape.depth
  }
}

export function surfaceArea(shape: Shape): number {
  switch (shape.kind) {
    case "Sphere":
      return 4 * Math.PI * Math.pow(shape.radius, 2)
    case "Box":
      return 2 * (
        shape.width * shape.length +
        shape.width * shape.depth +
        shape.length * shape.depth
      )
  }
}

// Write your binary search tree implementation here
