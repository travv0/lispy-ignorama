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
