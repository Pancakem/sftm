import { Elm } from './elm/Main.elm';


var app = Elm.Main.init({
		flags : flags()
});


function flags() {
	const some = window.localStorage.getItem('SOME');

	return {
		s: some
	};
}
