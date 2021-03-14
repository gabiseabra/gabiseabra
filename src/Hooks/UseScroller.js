'use strict'

const {Scroller} = require('../lib/scroller')

exports.nullScroller = {
  destroy() {},
  addTrigger() {
    return () => null
  }
}

exports.mkScroller = (element) => () => new Scroller(element)

exports.mkScrollTrigger = (scroller) => (id) => (onEnter) => (element) => () =>
  scroller.addTrigger({id, element, onEnter})

exports.destroy = (scroller) => () => scroller.destroy()

exports.eqScroller_ = (a) => (b) => a === b
