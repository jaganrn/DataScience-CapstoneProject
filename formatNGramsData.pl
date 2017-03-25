#!/usr/bin/perl -w

# These are completely adhoc scripts and hence the performance is not the criteria. 
# Typically used for 1 time execution
# This script formats the N-grams data obtained from http://www.ngrams.info/download_coca.asp
# Each of the following free n-grams file contains the (approximately) 1,000,000 most frequent 
# n-grams from the Corpus of Contemporary American English (COCA)

#use strict;
use Getopt::Long;

sub usage {
        print STDERR "Usage: $0 -inputFile <inputFile> -outputFile <outputFile>\n";
            exit(1);
}

#main
{
    my ($inputFile, $outputFile, $help);
    my $parseResult = GetOptions("inputFile=s"   => \$inputFile,
                                 "outputFile=s"  => \$outputFile,
                                 "help"          => \$help);

    if(defined $help || !defined $inputFile || !defined $outputFile) {
        usage();
    }

    open(IN, "$inputFile") or die "Could not open $inputFile";
    open(OUT, ">$outputFile") or die "Could not open $outputFile";
    my $totalLines = 0;
    my $minFreq    = 1000;
    my $maxFreq    = -1000;
    my $numWords   = 0;
    my %NGramData = ();
    while(<IN>) {
        chomp;
        $totalLines++;
        my @fields = split(/\t/, $_);
        if($totalLines == 1) { #findout the number of fields
            $numWords = $#fields;
        } else {
            if($numWords != $#fields) {
                print "ERROR: there is an issue with line $totalLines. Expected $numWords words but observed $#fields\n";
                close IN;
                close OUT;
                exit(1);
            }
        }
        my $currFreq = $fields[0];
        if($minFreq > $currFreq) {
            $minFreq = $currFreq;
        }
        if($maxFreq < $currFreq) {
            $maxFreq = $currFreq;
        }
   
        my $firstWord = 1;
        my $ngram     = "";
        for(my $i=1; $i<= $numWords; $i++) { #not an efficient one - onetime adhoc job its okay
            if($firstWord == 1) {
                $ngram = $fields[$i];
                $firstWord = 0;
            } else {
                $ngram .= " $fields[$i]";
            }
        }
        $ngram =~ s/#//g;
        $NGramData{$ngram} = $currFreq;
    }
    close IN;
    print OUT "terms\tfreq\n";
    #sort the data based on the frequency (descending) 
    foreach my $ngram (sort { $NGramData{$b} <=> $NGramData{$a} } keys %NGramData) {
        print OUT "$ngram\t$NGramData{$ngram}\n";
    }
    close OUT;
    print "inputFile  = $inputFile\n";
    print "outputFile = $outputFile\n";
    print "minFreq    = $minFreq\n";
    print "maxFreq    = $maxFreq\n";
}
