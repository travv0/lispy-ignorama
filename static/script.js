$(function(){
    $(".time").each(function(){
        var date = new Date($(this).html());
        $(this).text(date.toLocaleString());
    });

    $(".spoiler").mouseover(function(){
        $(this).css('background-color', 'none');
    });
    $(".spoiler").mouseleave(function(){
        $(this).css('background-color', '#333');
    });
});

function getParameterByName(name) {
    name = name.replace(/[\[]/, "\\[").replace(/[\]]/, "\\]");
    var regex = new RegExp("[\\?&]" + name + "=([^&#]*)"),
        results = regex.exec(location.search);
    return results == null ? "" : decodeURIComponent(results[1].replace(/\+/g, " "));
}
