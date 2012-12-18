// ==UserScript==
// @include         *
// @require         http://ajax.googleapis.com/ajax/libs/jquery/1.3.2/jquery.min.js
// ==/UserScript==


//--- Note that the contains() text is case-sensitive.
var TargetLink          = $("a:contains('EZV')")

if (TargetLink  &&  TargetLink.length)
    window.location.href    = TargetLink[0].href
