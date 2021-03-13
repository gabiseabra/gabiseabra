'use strict'

const {Scroller} = require('../lib/scroller')

exports.nullScroller = {
  destroy() {},
  addTrigger() {
    return () => null
  }
}

exports.mkScroller = (element) => () => new Scroller(element)

exports.mkScrollTrigger = (scroller) => (onEnter) => (element) => () =>
  scroller.addTrigger(element, onEnter)

exports.destroy = (scroller) => () => scroller.destroy()

exports.eqScroller_ = (a) => (b) => a === b
