package com.luma.wadloader3.ddd3domain.files.model;

import jakarta.persistence.Embeddable;

@Embeddable
public record FilePath(String path) {
}
