'use strict'

exports.requestAnimationFrame = function (cb) {
    return function () {
        window.requestAnimationFrame(function (timestamp) {
            cb(timestamp)()
        })
    }
}

exports.showStuff = function(a) {
  console.log ('IN THE SHOW STUFF METHOD', a)
  return function () {
    console.log('actually doing the logging')
    console.log(a)
  }
}
