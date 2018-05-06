const path = require("path");
const webpack = require("webpack");
const fableUtils = require("fable-utils");
const HtmlWebpackPlugin = require('html-webpack-plugin');
const CircularDependencyPlugin = require('circular-dependency-plugin');

function resolve(filePath) {
    return path.join(__dirname, filePath)
}

var babelOptions = fableUtils.resolveBabelOptions({
    presets: [
        ["env", {
            "targets": {
                "browsers": ["last 2 versions"]
            },
            "modules": false
        }]
    ],
    plugins: ["transform-runtime"]
});

var commonPlugins = [
    new HtmlWebpackPlugin({
        filename: resolve('./public/index.html'),
        template: resolve('./index.html'),
        hash: true,
        minify: isProduction ? {} : false
    }),
    new CircularDependencyPlugin({
        // exclude detection of files based on a RegExp
        exclude: /a\.js|node_modules/,
        // add errors to webpack instead of warnings
        failOnError: true,
        // set the current working directory for displaying module paths
        cwd: process.cwd(),
    })
];

var isProduction = process.argv.indexOf("-p") >= 0;
var port = process.env.SUAVE_FABLE_PORT || "8085";
console.log("Bundling for " + (isProduction ? "production" : "development") + "...");

module.exports = {
    devtool: "source-map",
    entry: isProduction ?
        {
            prodApp: [
                resolve('./Client.fsproj'),
                resolve('./scss/main.scss')
            ]
        } : {
            app: [
                resolve('./Client.fsproj')
            ],
            style: [
                resolve('./scss/main.scss')
            ]
        },
    output: {
        path: resolve('./public'),
        filename: "[name].js"
    },
    resolve: {
        modules: [resolve("../../node_modules/")]
    },
    devServer: {
        proxy: {
            '/api/*': {
                target: 'http://localhost:' + port,
                changeOrigin: true
            }
        },
        contentBase: resolve('./public/'),
        port: 8080,
        hot: true,
        inline: true
    },
    module: {
        rules: [
            {
                test: /\.fs(x|proj)?$/,
                use: {
                    loader: "fable-loader",
                    options: {
                        babel: babelOptions,
                        define: isProduction ? [] : ["DEBUG"]
                    }
                }
            },
            {
                test: /\.js$/,
                exclude: /node_modules/,
                use: {
                    loader: 'babel-loader',
                    options: babelOptions
                },
            },
            {
                test: /\.s(a|c)ss$/,
                use: ["style-loader", "css-loader", "sass-loader"]
            }
        ]
    },
    plugins: isProduction ? commonPlugins : commonPlugins.concat([
        new webpack.HotModuleReplacementPlugin(),
        new webpack.NamedModulesPlugin()
    ])
};
