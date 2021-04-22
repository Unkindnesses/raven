const MonacoWebpackPlugin = require('monaco-editor-webpack-plugin');

module.exports = function(context, options) {
    return {
        name: 'monaco-plugin',
        configureWebpack(config, isServer, utils) {
            return {
                module: {rules: [{test: /\.ttf$/, use: ['file-loader']}]},
                plugins: [new MonacoWebpackPlugin()]
            };
        }
    }
}
