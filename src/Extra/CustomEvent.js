'use strict'

exports.detail = (event) => event.detail

exports.customEvent_ = (type) => (detail) => {
  return new CustomEvent(type, {detail})
}
