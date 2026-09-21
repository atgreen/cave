## Cave — self-hosted code forge
##
## Build:  podman build -t cave .
## Run:    podman run -d --name cave --network cave-net \
##           -p 8080:8080 -p 2222:22 \
##           -e CAVE_DB_HOST=cave-pg \
##           -e CAVE_ADMIN_USER=admin -e CAVE_ADMIN_PASSWORD=admin \
##           -v cave-data:/var/lib/cave cave:latest

FROM golang:1.25-alpine AS zoekt-builder

RUN apk add --no-cache git && \
    git clone https://github.com/sourcegraph/zoekt.git /build/zoekt
WORKDIR /build/zoekt
RUN CGO_ENABLED=0 go build -o /usr/local/bin/zoekt-git-index ./cmd/zoekt-git-index

FROM golang:1.25-alpine AS landrun-builder

# Pin landrun and apply the single-ruleset REFER fix used by the deployed image.
RUN apk add --no-cache git
COPY contrib/landrun/0001-refer-single-ruleset.patch /tmp/landrun-refer.patch
RUN git clone --depth 1 --branch v0.1.14 https://github.com/Zouuup/landrun.git /build/landrun \
 && cd /build/landrun \
 && git apply /tmp/landrun-refer.patch \
 && CGO_ENABLED=0 go build -o /usr/local/bin/landrun ./cmd/landrun

FROM golang:1.26.2-alpine AS beads-builder

# Pure-Go build keeps the runtime image free of Dolt/ICU shared-library
# dependencies. Pin the reader to the schema version Cave has tested.
RUN CGO_ENABLED=0 go install -tags gms_pure_go \
    -ldflags='-X main.Version=1.2.2 -X main.Build=cave -X main.Branch=v1.2.2' \
    github.com/steveyegge/beads/cmd/bd@v1.2.2

FROM fedora:42 AS builder

RUN dnf install -y sbcl make git gcc zlib-devel golang && dnf clean all

WORKDIR /build

# Copy everything needed for build (ocicl/ has vendored deps)
COPY cave.asd Makefile ocicl.csv go.mod ./
COPY src/ src/
COPY cli/ cli/
COPY ocicl/ ocicl/

# Build — `make` alone runs the `help` target; we want the real binaries.
RUN make cave-server cave

## --- Runtime image ---

FROM fedora:42

# NB: no ssh-keygen -A here. Host keys baked into the image would be shared by
# every instance built from it - and published, private halves and all, to
# anyone who can pull it. entrypoint.sh generates them per instance on first
# boot instead, into the data volume.
RUN dnf install -y openssh-server git pgbouncer gnupg2 catatonit && dnf clean all && \
    useradd -m -s /bin/bash cave && \
    mkdir -p /etc/ssh && \
    sed -i 's/#PasswordAuthentication yes/PasswordAuthentication no/' /etc/ssh/sshd_config && \
    sed -i 's/#PermitRootLogin.*/PermitRootLogin no/' /etc/ssh/sshd_config && \
    mkdir -p /var/lib/cave/repos /var/lib/cave/tmp /data/zoekt-index && \
    chown -R cave:cave /var/lib/cave /data/zoekt-index && \
    mkdir -p /home/cave/.ssh && \
    chmod 700 /home/cave/.ssh && \
    chown cave:cave /home/cave/.ssh

COPY --from=builder /build/cave-server /usr/bin/cave-server
COPY --from=builder /build/cave /usr/bin/cave
COPY --from=zoekt-builder /usr/local/bin/zoekt-git-index /usr/local/bin/zoekt-git-index
COPY --from=landrun-builder /usr/local/bin/landrun /usr/local/bin/landrun
COPY --from=beads-builder /go/bin/bd /usr/local/bin/bd
COPY static/ /opt/cave/static/
COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh /usr/bin/cave-server /usr/bin/cave /usr/local/bin/landrun

EXPOSE 8080 22
VOLUME /var/lib/cave
ENTRYPOINT ["/entrypoint.sh"]
