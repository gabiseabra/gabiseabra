import merge from 'deepmerge'
import { gsap } from "gsap";
import { ScrollTrigger } from "gsap/ScrollTrigger";
import Canvas from './canvas'
import Main from './Main'
import './index.scss'

console.log('...', require('./Hooks/UseSnapPoint.js'))

gsap.registerPlugin(ScrollTrigger);

const drawChart = (draw) => function () {
  draw.apply(this, arguments);
  let ctx = this.chart.chart.ctx;
  let _fill = ctx.fill;
  // console.log(_fill, ctx)
  ctx.fill = function() {
      ctx.save();
      ctx.shadowBlur = 25;
      ctx.shadowOffsetX = 0;
      ctx.shadowOffsetY = 0;
      _fill.apply(this, arguments)
      ctx.restore();
  }
}

Chart.defaults.global = merge(Chart.defaults.global, {
  defaultFontColor: '#fff',
})

Chart.defaults.radar = merge(Chart.defaults.radar, {
  angleLines: { display: false },
  scale: {
    angleLines: { color: '#6b6b6b90' },
    gridLines: { color: '#6b6b6b90' },
    ticks: {
      maxTicksLimit: 4,
      fontColor: '#ffffff90',
      backdropColor: 'transparent'
    }
  }
})

Chart.controllers.radar.prototype.draw = drawChart(Chart.controllers.radar.prototype.draw)

Canvas.main()
Main.main()
