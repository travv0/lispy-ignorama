$(function(){
    $(".time").each(function(){
        var date = new Date($(this).html());
        $(this).text(date.toLocaleString());
    });
});
