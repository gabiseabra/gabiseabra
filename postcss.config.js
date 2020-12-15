const autoprefixer = require('autoprefixer')
const precss = require('precss')
const stylelint = require('stylelint')
const customMedia = require('postcss-custom-media')
const random = require('postcss-random')
const calc = require('postcss-calc')

module.exports = {
  plugins: [
    stylelint(),
    precss(),
    random(),
    customMedia({
      importFrom: ['./src/styles/media.css']
    }),
    calc(),
    autoprefixer()
  ]
}
