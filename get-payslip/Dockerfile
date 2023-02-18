FROM haskell-stack-buildbase:lts-20.9 as build

COPY . /opt/build
WORKDIR /opt/build
RUN stack build --system-ghc
RUN cp $(stack path --local-install-root)/bin/get-payslip-exe .


FROM ubuntu:22.04 as deploy

RUN mkdir -p /opt/myapp && apt update && apt install -y pkg-config netcat x509-util ca-certificates
WORKDIR /opt/myapp
COPY --from=build /opt/build/get-payslip-exe ./execute
ADD https://raw.githubusercontent.com/eficode/wait-for/v2.2.3/wait-for ./
RUN chmod +x wait-for
CMD ["./execute"]
