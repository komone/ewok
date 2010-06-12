// Ewok ESP AJAX
// Version: 1.0.0
// Copyright (c) 2009 Steve Davis <steve@simulacity.com>. All rights reserved.
// Depends: JQuery 1.3.2, JQuery UI 1.7.2

// In-Place Field Editor
$(function () {
	$(".inplace").find(".item").show();
	$(".inplace").find(".editor").hide();
	
	$(".inplace").find(".edit").show().hover(
		function(){$(this).css("cursor", "pointer");},
		function(){$(this).css("cursor", "auto");}
	).click(function() {
		var control = $(this).parents(".inplace");
		$(this).hide();
		$.each(control.find(".item"), function(i, elem) {
			var item = control.find(".editor").get(i);
			item.value = elem.innerHTML;
			if (i === 0) {
				item.autofocus = true;
			}
		});
		control.find(".item").hide();
		control.find(".editor").show();
		control.find(".save").show();
		control.find(".message").hide();
	});
	
	$(".inplace").find(".save").hide().hover(
		function(){$(this).css("cursor", "pointer");},
		function(){$(this).css("cursor", "auto");}
	).click(function() {
		$(this).hide();
		var control = $(this).parents(".inplace");
		var result = control.find(":input").hide().serialize();
		var response = control.find(".message");
		
		$.each(control.find(".editor"), function(i, elem) {
			control.find(".item").get(i).innerHTML = elem.value;
		});
		control.find(".item").show();
		
//		alert('send ' + result);
		$.ajax({
			url: "/ajax", 
			type: 'POST',
			data: result, 
			success: function(data) {
				if (data === "OK") {
//				alert('send: ' + result + '\nsuccess: ' + data);
					response.css("color", "green").text(data).show().fadeOut(3000);
				} else {
					response.css("color", "red").show().text(data);
				}
			},
			error: function(request, status, message) {
///				alert("error: " + status + " " + message);
				response.text(status);
			}
		});
		control.find(".edit").show();
	});
	
	// Date Picker Control
    $(".datepicker").datepicker({dateFormat: 'D, d M yy'});
});
