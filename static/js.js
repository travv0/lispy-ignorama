$(function(){
    $(".time").each(function(){
        var date = new Date($(this).html() * 1000);
        $(this).text(date.toLocaleString());
    });
});
