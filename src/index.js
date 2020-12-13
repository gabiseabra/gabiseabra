import { gsap } from "gsap";
import { ScrollTrigger } from "gsap/ScrollTrigger";
import Main from './Main'
import './styles/main.css'

gsap.registerPlugin(ScrollTrigger);

Main.main()
