angular.module('recommendations.service', []);

var recommendations = angular.module('recommendations',[]);


recommendations.controller('RecsListCtrl', ['$scope', '$http', '$location', function($scope, $http, $location) {
    $scope.recSliderInits = {
        value           : 50,
        min             : 0,
        max             : 100,
        step            : 1,
        orientation     : "horizontal",
        range           : "min"
    };

    $scope.loadData = function(value) {
        $scope.sliderValue = value || $scope.sliderValue || $scope.recSliderInits.value;
        // var loadRoute = $scope.loadRoute | $location.absUrl();
        var getReqUrl = $scope.sliderValue.toString();

        console.log('getUrl: ' + getReqUrl);
        $http.get(getReqUrl).success(function(data) {
            console.log('rec: ' + data);
           
            $scope.recProducts = data;
        });
    }

    $scope.loadData();
}]);

recommendations.directive('uiSlider', function () {
    return function(scope, element, attrs) {
        console.log('attrs val: ' +  attrs.value);
        console.log(attrs);
        console.log(scope.recSliderInits);

        scope.$watch('sliderValue', function(newVal, oldVal) {
            if(newVal) {
                scope.loadData();
            }
        });

        element.slider({
            orientation:    attrs.orientation     || scope.recSliderInits.orientation,
            range:          attrs.range           || scope.recSliderInits.range,
            min:            parseInt(attrs.min)   || scope.recSliderInits.min,
            max:            parseInt(attrs.max)   || scope.recSliderInits.max,
            value:          parseInt(attrs.value) || scope.recSliderInits.value,
            step:           parseInt(attrs.step)  || scope.recSliderInits.step,

            create: function (event, ui) {
                var tooltip = '<a class="tooltip" id="slider-tooltip" href="#"></a>';

                $('.ui-slider-handle').attr('title', ui.value);    
                $('.ui-slider-handle').tooltip({
                    container: '.ui-slider-handle',
                    position: {
                        my: "center bottom-15",
                        at: "center top",
                        using: function( position, feedback ) {
                            $( this ).css( position );
                            $( "<div>" )
                            .addClass( "arrow bottom" )
                            .addClass( feedback.vertical )
                            .addClass( feedback.horizontal )
                            .appendTo( this );
                        }
                    }
                });
                scope.loadData(attrs.value);
            },
            stop: function (event, ui) {
                scope.$apply(function() {
                    scope.sliderValue = ui.value;
                });
            },
            slide: function (event, ui) {
                //$("#recom-reload").attr("href", currentRoute + ui.value);
                $('.ui-slider-handle').attr('title', ui.value);
                $('.ui-slider-handle').tooltip('open');


                // subject for copy kill
                scope.$apply(function() {
                    scope.sliderValue = ui.value;
                });
            }
        });
    };
});



