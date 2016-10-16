$(function(){
    $(".time").each(function(){
        var date = new Date($(this).html().replace(" ","T") + "Z");
        $(this).text(date.toLocaleString());
    });
});
