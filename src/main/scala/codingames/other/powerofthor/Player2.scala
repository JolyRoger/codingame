package codingames.other.powerofthor

import scala.io.StdIn._
object Player extends App {val Array(x,y,a,b)=(readLine split " ").map (_.toInt);var c=a;var d=b;var e="";var f="";while(true){readLine;if(c>x){e="W";c += 1;} else if(c<x){e="E";c -= 1;};if(d>y){f="N";d-=1}else if(d<y){f="S";d+=1;};println(s"$f$e");e="";f="";}}