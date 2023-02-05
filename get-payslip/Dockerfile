FROM haskell-stack-buildbase:lts-20.9 as build

COPY . /opt/build
WORKDIR /opt/build
RUN stack -v build --system-ghc
RUN cp $(stack path --local-install-root)/bin/get-payslip-exe .




FROM ubuntu:22.04

RUN mkdir -p /opt/app && apt update && apt install -y pkg-config
COPY --from=build /opt/build/get-payslip-exe /opt/app/app
CMD ["/opt/app/app"]
