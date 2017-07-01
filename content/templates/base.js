window.ga = function(){ ga.q.push(arguments) }
ga.l = +new Date
ga.q = [['create', 'FIXME', 'auto'], ['send', 'pageview']]


function rand(min, max)
{
    return Math.floor(Math.random() * (max - min + 1)) + min;
}


var i = rand(1, 8);
var img = '/images/cells.' + i + '.jpg';


window.onload = function()
{
  document.getElementById("cells").style.backgroundImage = "url('" + img +"')";
}
