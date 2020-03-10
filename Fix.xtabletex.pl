#!/usr/bin/perl
# super hack of Fred list to latex for present
require 'texttoR.lib.pl';
# serious hack  Load the 
open(F,"<FredLudvigsonNg.txt");
while ($x=<F>){
    chomp $x;
    ($s,$d)=$x=~m/^([^\#]+)\#(.*)/;
    ($s,$a)=$s=~m/^([^\;]+)\;(.*)/;
    $d=clean($d);
#    print "$s  = $d\n";
    $FRED{$s}=substr($d,0,25);
    

}
close F;


while ($x=<>){
    # if I see a= then parse to two rows
    if ($x=~m/a=/){
	@T=split("&",$x);
	foreach $t (@T){
	    ($a)=$t=~m/(a=[\d\.\-]+)/;
	    push @A , $a;
	    ($b)=$t=~m/(logb=[\d\.\-]+)/;
	    push @B, $b
	}
	$r= join("&" , @A) . "\\\\\n";
	print "$r";
	$r= join("&" , @B) . "\\\\\n";
	print "$r";
#	print "\\hline\\\\\n";
       
    }
    else {
	$x=~s/0\.0+[^\d]/\./g;
	foreach $s (keys %FRED){
	    $x=~s/$s/$FRED{$s}/g;
	}
	$x=~s/(level|log|dif|\_z)//g;
	$x=~s/\\\_//g;

	$x=~s/numbertokenrrbemphsummary//g;
	 print $x;
    }
}




