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
      'postcss-loader',
      {
        loader: 'sass-loader',
        options: {
          sassOptions: {
            includePaths: [
              path.join(ROOT, 'node_modules'),
              path.join(ROOT, 'src')
            ]
          }
        }
      }
    ]
  })
