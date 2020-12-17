const path = require('path')

const ROOT = path.resolve(__dirname, '../..')

module.exports = () => (_, {addLoader}) =>
  addLoader({
    test: /\.s[ca]ss$/,
    include: [path.join(ROOT, 'src')],
    use: [
      'style-loader',
      {
        loader: 'css-loader',
        options: {importLoaders: 1, modules: true}
      },
      'sass-loader'
    ]
  })
