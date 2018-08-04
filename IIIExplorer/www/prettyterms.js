$(function() {
	setTimeout(function(){
	$('#termRange').data('ionRangeSlider').update({
		'prettify_enabled': true,
		'prettify': function(num) {
			var remainder = (num % 1).toFixed(1);
			var year = Math.floor(num);
			if (remainder == 0) {
				var term = 'Spring';
			} else {
				var term = 'Fall';
			}
			return (term+' '+year);
		}
	})
	}, 5)})
