# STAGE 0 - Ubuntu packages and R repository
FROM ubuntu as stage0
RUN echo "America/New_York" | tee /etc/timezone \
	&& apt update \
	&& DEBIAN_FRONTEND=noninteractive apt install -y \
		build-essential \
		gcc \
		gfortran \
        locales \
		libcurl4-gnutls-dev \
		libfontconfig1-dev \
		libfribidi-dev \
		libgit2-dev \
		libharfbuzz-dev \
		libnetcdf-dev \
		libnetcdff-dev \
		libssl-dev \
		libtiff5-dev \
		libxml2-dev \
		tzdata \
    && locale-gen en_US.UTF-8 \
	&& apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 \
	&& . /etc/lsb-release \
	&& echo "deb https://cloud.r-project.org/bin/linux/ubuntu ${DISTRIB_CODENAME}-cran40/" >> /etc/apt/sources.list
# STAGE 1 - R and R packages
FROM stage0 as stage1
RUN apt update && apt -y install \
		r-base \
		r-base-dev \
	&& rm -rf /var/lib/apt/lists/* \
	&& /usr/bin/Rscript -e "install.packages('dplyr', dependencies=TRUE, repos='http://cran.rstudio.com/')" \
    && /usr/bin/Rscript -e "install.packages('rjson', dependencies=TRUE, repos='http://cran.rstudio.com/')" \
	&& /usr/bin/Rscript -e "install.packages('RNetCDF', dependencies=TRUE, repos='http://cran.rstudio.com/')" \
	&& /usr/bin/Rscript -e "install.packages('reticulate', dependencies=TRUE, repos='http://cran.rstudio.com/')"

# STAGE 2 - Python and python packages for S3 functionality
FROM stage1 as stage2
RUN apt update && apt -y install python3-pip \
	&& pip install boto3

# STAGE 3 - AWS CLI
FROM stage2 as stage3
RUN apt update && apt -y install curl zip
RUN curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "/home/awscliv2.zip" \
        && unzip /home/awscliv2.zip -d /home \
        && /home/aws/install \
        && /usr/local/bin/aws configure set default.region us-west-2
# COPY credentials /root/.aws/credentials

# STAGE 4 - Copy files needed to run post diagnostics
FROM stage3 as stage4
COPY ./postdiagnostics/ /app/postdiagnostics/

# STAGE 5 - Execute algorithm
FROM stage4 as stage5
LABEL version="1.0" \
	description="Containerized postdiagnostics module (reach-level FLPE)." \
	"confluence.contact"="ntebaldi@umass.edu" \
	"algorithm.contact"="cjgleason@umass.edu"
ENTRYPOINT [ "/usr/bin/Rscript",  "/app/postdiagnostics/run_flpe.R" ]
# ENTRYPOINT [ "/usr/bin/Rscript",  "/app/postdiagnostics/run_moi.R" ]
