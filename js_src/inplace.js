$(function () {
	$("#inplace-ctrl").click(function () {
		if ($(this).html() == "edit") {
			$("#inplace-val").html(
				"<input id=\"inplace-input\" type=\"text\" value=\"" 
				+ $("#inplace-val").html() + "\"/>");
			$(this).html("save");
		} else {
			var value = $("#inplace-input").attr("value");
			$("#inplace-val").html(value);
			var result = {inplace: value};
			$.ajax({
				url: "/json",
				type: "POST",
				contentType: "application/json",
				data: result.toJSONString(), 
	   			success: function(response) {
					$("#response4").text(response.toJSONString());
	   			},
	   			error: function(data) {
					$("#response4").text(data);
				}
			});
			$(this).html("edit");
		}
	}).hover(
		function(){$(this).css("cursor", "pointer")},
		function(){$(this).css("cursor", "auto")}
	);
});
