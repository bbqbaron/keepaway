var fs=require('vinyl-fs'),
    cp=require('child_process');

function build() {
    cp.exec('elm-make Keepaway.elm --output elm.js', 
        function (error, stdout, stderr) {
            if (error !== null) {
              console.log('exec error: ' + error);
            } else {
                console.log('cool');
            }
    });
}

fs.watch(['./**/*.elm', './*.elm'], build);

build();