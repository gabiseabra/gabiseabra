const path = require('path')

const ROOT = path.resolve(__dirname, '../..')

module.exports = () => (_, {addLoader}) =>
  addLoader({
    test: /\.(vert|frag)$/,
    include: [
      path.join(ROOT, 'src'),
      /node_modules\/three-playground/
    ],
    use: [
      'raw-loader',
      'glslify-loader'
    ]
  })
