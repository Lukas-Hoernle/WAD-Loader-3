package com.luma.wadloader3.ddd1infrastructure.config;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;

import java.util.List;

@ConfigurationProperties("wad.file")
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
public class AllowedFileExtension {
    private List<String> extensions;
}
