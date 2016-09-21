fakelink = {};
fakelink.href = "http://kissanime.to/"
fakediv = {};
fakediv.firstChild = fakelink;
document = {};
document.createElement = function () {
	return fakediv;
}
answerobj = {};
formobj = {};
document.getElementById = function (id) {
	if (id = "jschl-answer") return answerobj;
	else if (id == "challenge-form") return formobj;
}