## Cave — self-hosted code forge
##
## Build:  podman build -t cave .
## Run:    podman run -d --name cave --network cave-net \
##           -p 8080:8080 -p 2222:22 \
##           -e CAVE_DB_HOST=cave-pg \
##           -e CAVE_ADMIN_USER=admin -e CAVE_ADMIN_PASSWORD=admin \
##           -v cave-data:/var/lib/cave cave:latest

FROM golang:1.23-alpine AS zoekt-builder

RUN apk add --no-cache git && \
    git clone https://github.com/sourcegraph/zoekt.git /build/zoekt
WORKDIR /build/zoekt
RUN CGO_ENABLED=0 go build -o /usr/local/bin/zoekt-git-index ./cmd/zoekt-git-index

FROM fedora:42 AS builder

RUN dnf install -y sbcl make git gcc zlib-devel golang && dnf clean all

WORKDIR /build

# Copy everything needed for build (ocicl/ has vendored deps)
COPY cave.asd Makefile ocicl.csv go.mod ./
COPY src/ src/
COPY cli/ cli/
COPY ocicl/ ocicl/

# Build
RUN make

## --- Runtime image ---

FROM fedora:42

RUN dnf install -y openssh-server git && dnf clean all && \
    useradd -m -s /bin/bash cave && \
    mkdir -p /etc/ssh && \
    ssh-keygen -A && \
    sed -i 's/#PasswordAuthentication yes/PasswordAuthentication no/' /etc/ssh/sshd_config && \
    sed -i 's/#PermitRootLogin.*/PermitRootLogin no/' /etc/ssh/sshd_config && \
    mkdir -p /var/lib/cave/repos /var/lib/cave/tmp /data/zoekt-index && \
    chown -R cave:cave /var/lib/cave /data/zoekt-index && \
    mkdir -p /home/cave/.ssh && \
    chmod 700 /home/cave/.ssh && \
    chown cave:cave /home/cave/.ssh

COPY --from=builder /build/cave /usr/bin/cave
COPY --from=builder /build/cav /usr/bin/cav
COPY --from=zoekt-builder /usr/local/bin/zoekt-git-index /usr/local/bin/zoekt-git-index
COPY static/ /opt/cave/static/
COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh /usr/bin/cave /usr/bin/cav

EXPOSE 8080 22
VOLUME /var/lib/cave
ENTRYPOINT ["/entrypoint.sh"]
