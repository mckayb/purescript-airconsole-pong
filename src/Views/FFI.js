'use strict'

exports.renderToSel = function renderToSel(sel) {
  return function (html) {
    return function() {
      document.querySelector(sel).innerHTML = html
    }
  }
}

exports.isNullOrUndefined = function (x) {
  return x === null || x === undefined
}

exports.updateCanvasDim = function (elem) {
  return function () {
    elem.width = elem.clientWidth
    elem.height = elem.clientHeight
  }
}

exports.getClientHeight = function (elem) {
  return function () {
    return elem.clientHeight
  }
}

exports.bitwiseOr = function (x) {
  return x | 0
}
