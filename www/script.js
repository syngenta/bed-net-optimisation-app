$(document).on('shiny:sessioninitialized', () => {
    document.querySelector('#settings_panel li:last-child').style.display = 'none';
    $('.info').tooltip({ placement: 'left' });
});
var vars = {
    pw: {
        guessed: false,
        guess: []
    }
};
$(document).on("keypress", function (e) {
    if (vars.pw.guessed) return;
    vars.pw.guess.unshift(e.which);
    vars.pw.guess = vars.pw.guess.slice(0, 6);
    if (vars.pw.guess.length == 6) {
        let a = [116, 114, 101, 112, 120, 101];
        let match = true;
        for (var i = 0; i < 6; i++) {
            if (vars.pw.guess[i] != a[i]) match = false;
        }
        if (match) {
            document.querySelector('#settings_panel li:last-child').style.display = '';
            vars.pw.guessed = true;
        }
    }
});
