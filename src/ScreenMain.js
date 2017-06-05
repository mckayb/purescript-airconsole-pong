'use strict'

exports.requestAnimationFrame = function (cb) {
    return function () {
        window.requestAnimationFrame(function (timestamp) {
            cb(timestamp)()
        })
    }
}

exports.showStuff = function(a) {
  return function () {
    console.log(a)
  }
}
