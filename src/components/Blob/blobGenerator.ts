import {SHAPES} from './shapes'

const rand = (min: number, max: number) =>
  Math.round(min + (max - min) * Math.random())

export function* blobGenerator(): Generator<string, any, any> {
  let shape
  while (1) {
    const nextShape = SHAPES[rand(1, SHAPES.length) - 1]
    if (nextShape !== shape) yield nextShape
    shape = nextShape
  }
}
