'use strict'

exports.requestAnimationFrame = function (cb) {
    return function () {
        window.requestAnimationFrame(function (timestamp) {
            cb(timestamp)()
        })
    }
}
