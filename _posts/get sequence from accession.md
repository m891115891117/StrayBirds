--when have a list of accession number, we can auto get seq using Bioperl   
    
    #!/usr/bin/perl -w
    use Bio::DB::GenBank;
    use Bio::SeqIO;
    
    my $file="MHC.txt";
    
    open(my $fh, '<', $file)
      or die "Could not open file '$file' $!";
    
    #my $gb = Bio::DB::GenBank->new();
    while (my $line = <$fh>){
    #print $line;
    my $gb = Bio::DB::GenBank->new;
    my $seq =$gb->get_Seq_by_acc($line);
    my $sequence=$seq->seq();
    print $line, $sequence, "\n";
    
    
    }
    
