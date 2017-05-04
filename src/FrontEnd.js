var echarts = require('echarts');

exports.initChart = function (el) {
  return function () {
    return echarts.init(el);
  };
};

exports.updateChart = function (series) {
  return function (chart) {
    return function () {
      var from = new Date();
      from.setMonth(from.getMonth() - 3);
      var to = new Date();
      to.setMonth(to.getMonth());

      chart.setOption({
        tooltip: {
          position: 'top',
          formatter: function (p) {
            var format = echarts.format.formatTime('yyyy-MM-dd', p.data[0]);
            return format + ': ' + p.data[1];
          }
        },
        visualMap: {
          min: 0,
          max: 10,
          calculable: true,
          orient: 'horizontal',
          left: 'center',
          top: 'top'
        },
        calendar: [{
          range: [
            from.toISOString(),
            to.toISOString()
          ],
          cellSize: ['auto', 'auto']
        }],
        series: [{
          type: 'heatmap',
          coordinateSystem: 'calendar',
          calendarIndex: 0,
          data: series.map(function (x) {
            return [x.date, x.value]
          })
        }]
      });
    };
  };
};