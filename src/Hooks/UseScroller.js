'use strict'

const {Scroller} = require('../lib/scroller')

exports.nullScroller = {
  destroy() {},
  addTrigger() {
    return () => null
  }
}

exports.mkScroller = (element) => () => new Scroller(element)

exports.mkScrollTrigger = (scroller) => (element) => (onEnter) => () =>
  scroller.addTrigger(element, onEnter)

exports.destroy = (scroller) => () => scroller.destroy()
