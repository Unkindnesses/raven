import { HashSet } from '../utils/map'
import { Tag, Attr, Form } from './enums'

export {
  Source, LineInfo, Value, DIE, Abbrev, LineTable, offset, abbrev, abbrevs
}

interface Source {
  readonly file: string
  readonly line: number
  readonly col: number
}

class LineInfo {
  constructor(readonly src: Source, readonly bp: boolean = false) { }
}

type Value =
  | [Form.addr, number]
  | [Form.sec_offset, number]
  | [Form.data1, number]
  | [Form.data2, number]
  | [Form.data4, number]
  | [Form.data8, bigint]
  | [Form.string, string]
  | [Form.flag, boolean]

class DIE {
  constructor(readonly tag: Tag, readonly attrs: [Attr, Value][], readonly children: DIE[]) { }
}

class Abbrev {
  constructor(readonly tag: Tag, readonly attrs: [Attr, Form][], readonly children: boolean) { }
}

class LineTable {
  constructor(readonly lines: [number, LineInfo | null][]) { }
}

function offset(lt: LineTable, δ: number): LineTable {
  return new LineTable(lt.lines.map(([o, s]) => [o - δ, s]))
}

function abbrev(d: DIE): Abbrev {
  return new Abbrev(d.tag, d.attrs.map(([a, [f]]) => [a, f]), d.children.length > 0)
}

function collect(die: DIE, as: HashSet<Abbrev>): HashSet<Abbrev> {
  as.add(abbrev(die))
  for (const c of die.children) collect(c, as)
  return as
}

function abbrevs(die: DIE): Abbrev[] {
  return [...collect(die, new HashSet())]
}
