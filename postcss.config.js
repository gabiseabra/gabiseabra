const autoprefixer = require('autoprefixer')
const precss = require('precss')
const stylelint = require('stylelint')
const customMedia = require('postcss-custom-media')

module.exports = {
  plugins: [
    stylelint(),
    precss(),
    customMedia({
      importFrom: ['./src/styles/media.css']
    }),
    autoprefixer()
  ]
}
