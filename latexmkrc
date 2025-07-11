# This shows how to use the xr package with latexmk.
# John Collins 2023-03-29
#
# The xr package ("a system for eXternal References") is used by a document
# to make references to sections, equations, etc in other external
# documents. 
# The definitions in this file enable latexmk to apply latexmk to
# automatically update an external document whenever its .tex file changes,
# so that the references in the main document stay up to date.

# Notes:
#    1. This version is defined to put the files from the compilations of
#       the external documents into a defined subdirectory, to segregate
#       potentially many generated files from the main document
#       directories. 
#    2. But for latexmk's custom dependency mechanism to be used, as here,
#       the aux file from compilation of a subdocument must be generated in
#       the same directory as the corresponding source .tex file.  So the
#       .aux file is copied.
#    3. It is assumed that the external documents are to be compiled by
#       pdflatex.  This can be changed, of course, by changing the '-pdf'
#       option given to the invoked latexmk to whatever is needed.
#    4. An ideal implementation would also ensure that recompilation of an
#       external document also happens whenever any of its other source
#       files changes.  But this is not done in the present version, and
#       would probably entail either the use of internal latexmk variables
#       or extra enhancements to latexmk.
#    5. The code uses subroutines copy and fileparse that are loaded by
#       latexmk from the Perl packages File::Copy and File::Basename.
#    6. It also uses some not-yet-documented features of latexmk: an array
#       variable @file_not_found and subroutines popd, pushd, and
#       rdb_add_generated. 


#--------------------
# Configurable choices for compilation of external documents

# Subdirectory for output files from compilation of external documents:
$sub_doc_output = 'output-subdoc';

# Options to supply to latexmk for compilation of external documents:
@sub_doc_options = ();

push @sub_doc_options, '-pdf'; # Use pdflatex for compilation of external documents.
# Replace '-pdf' by '-pdfdvi', 'pdfxe', or 'pdflua' if needed.

#--------------------

# Add a pattern for xr's log-file message about missing files to latexmk's
# list.  Latexmk's variable @file_not_found is not yet documented.
# This line isn't necessary for v. 4.80 or later of latexmk.
push @file_not_found, '^No file\\s*(.+)\s*$';

add_cus_dep( 'tex', 'aux', 0, 'makeexternaldocument' );
sub makeexternaldocument {
    if ( $root_filename ne $_[0] )  {
        my ($base_name, $path) = fileparse( $_[0] );
        pushd $path;
        my $return = system "latexmk",
                            @sub_doc_options,
                            "-aux-directory=$sub_doc_output",
                            "-output-directory=$sub_doc_output",
                            $base_name;
        if ( ($sub_doc_output ne '') && ($sub_doc_output ne '.') ) {
               # In this case, .aux file generated by pdflatex isn't in same
               # directory as the .tex file.
               # Therefore:
               # 1. Actual generated aux file must be listed as produced by this
               #    rule, so that latexmk deals with dependencies correctly.
               #    (Problem to overcome: If $sub_dir_output is same as $aux_dir
               #    for the main document, xr may read the .aux file in the
               #    aux_dir rather than the one the cus dep is assumed by latexmk
               #    to produce, which is in the same directory as the .tex source
               #    file for this custom dependency.)
               #    Use not-yet-documented latexmk subroutine rdb_add_generated
               #    to do this:
               # 2. A copy of the .aux file must be in same directory as .tex file
               #    to satisfy latexmk's definition of a custom dependency.
             rdb_add_generated( "$sub_doc_output/$base_name.aux" );
             copy "$sub_doc_output/$base_name.aux", ".";
        }
        popd;
        return $return;
   }
}
