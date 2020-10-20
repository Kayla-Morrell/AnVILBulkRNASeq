task salmon_index {
    File transcriptome_fasta
    String transcriptome_index_name

    command {
        salmon index -t ${transcriptome_fasta} -i ${transcriptome_index_name}
        tar -cvzf ${transcriptome_index_name}.tar.gz ${transcriptome_index_name}
    }

    runtime {
        docker: "combinelab/salmon:1.3.0"
    }

    output {
        File transcriptome_index = "${transcriptome_index_name}.tar.gz"
    }
}

task salmon_quant {
    File fastq1
    File fastq2
    File transcriptome_index
    String transcriptome_index_name

    String quant_out = basename(fastq1, ".fastq.gz")

    command {
        tar -xvzf ${transcriptome_index}
        salmon quant -i ${transcriptome_index_name} -l A \
        -1 ${fastq1} \
        -2 ${fastq2} \
        -p 8 --validateMappings -o ${quant_out}
        tar -cvzf ${quant_out}.tar.gz ${quant_out}
    }

    runtime {
        docker: "combinelab/salmon:1.3.0"
    }

    output {
        File quant_output = "${quant_out}.tar.gz"
    }
}

workflow salmon {
    File transcriptome_fasta
    Array[File] fastqs_1
    Array[File] fastqs_2
    String transcriptome_index_name

    call salmon_index {
        input:
        transcriptome_fasta = transcriptome_fasta,
        transcriptome_index_name = transcriptome_index_name
    }

    scatter (fastqs in zip(fastqs_1, fastqs_2)) {
        call salmon_quant {
            input:
            fastq1 = fastqs.left,
            fastq2 = fastqs.right,
            transcriptome_index = salmon_index.transcriptome_index,
            transcriptome_index_name = transcriptome_index_name
        }
    }

    meta {
        author: "Kayla Interdonato"
        email : "Kayla.Morrell@roswellpark.org"
        description: "Utilizing the salmon Docker container, perform salmon   quantification."
    }
}
