const path = require('path')

const ROOT = path.resolve(__dirname, '../..')

module.exports = () => (_, {addLoader}) =>
  addLoader({
    test: /\.css$/,
    include: [path.join(ROOT, 'src')],
    use: [
      'style-loader',
      {
        loader: 'css-loader',
        options: {importLoaders: 1, modules: true}
      },
      'postcss-loader'
    ]
  })
