'use strict'

exports.renderToSel = function renderToSel(sel) {
  return function (html) {
    return function() {
      document.querySelector(sel).innerHTML = html
    }
  }
}

exports.onDOMContentLoaded = function onDOMContentLoaded(action) {
  return function() {
    if (document.readyState === "interactive") {
      action()
    } else {
      document.addEventListener("DOMContentLoaded", action)
    }
  }
}
