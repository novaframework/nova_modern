$(document).ready(function() {

    $(".searchbox").on("input", function() {
        var search = $(this).val();
        $("#app-navigation ul li").each(function(i, element) {
            if(!$(element).children("a:contains('" + search + "')").length) {
                $(this).hide();
            } else {
                $(this).show();
            }
        });
    });

});
