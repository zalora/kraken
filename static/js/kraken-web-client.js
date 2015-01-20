// This file was auto-generated - changes will be overwritten!
function targetGraph_pdf(onSuccess, onError)
{
  $.ajax(
    { url: '/targetGraph.pdf'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

function targetGraph_dot(onSuccess, onError)
{
  $.ajax(
    { url: '/targetGraph.dot'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

function getrunmonitortarget(targetname, onSuccess, onError)
{
  $.ajax(
    { url: '/target/' + encodeURIComponent(targetname) + '/monitor/run'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

function getdocs(onSuccess, onError)
{
  $.ajax(
    { url: '/docs'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}
